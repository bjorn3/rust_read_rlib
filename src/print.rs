use std::fmt;

use rustc::ty::TyCtxt;
use rustc::session::Session;
use rustc::middle::lang_items::LangItem;
use rustc::middle::exported_symbols::ExportedSymbol;
use rustc::hir::def::{Export, Def};
use rustc::hir::def_id::{DefId, CrateNum, DefIndex, DefIndexAddressSpace};
use rustc_metadata::cstore::{CrateMetadata, NativeLibraryKind};

use termion::color::*;
use clap::{App, SubCommand, Arg, ArgMatches};
use regex::Regex;

struct PrettyBool(bool);

impl fmt::Debug for PrettyBool {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            true => write!(f, "{}true{}", Fg(Green), Fg(Reset)),
            false => write!(f, "{}false{}", Fg(Red), Fg(Reset)),
        }
    }
}

macro_rules! header {
    ($the_string:expr) => {
        println!(concat!("\n{}", $the_string, "{}"), Fg(Yellow), Fg(Reset));
    };
}

macro_rules! out_line {
    ($name:expr, $fmt:expr, $($e:tt)*) => {
        println!(concat!("    {}{:<26}{}: ", $fmt), Fg(Cyan), $name, Fg(Reset), $($e)*);
    };
}

macro_rules! commands {
    ($($cmd:ident($($arg:ident),*) = $about:expr;)*) => {
        pub fn subcommands<'a, 'b>() -> Vec<App<'a, 'b>> where 'a: 'b {
            vec![
                $(SubCommand::with_name(stringify!($cmd))
                    .about($about)
                    $(
                        .arg(Arg::with_name(stringify!($arg)).required(true))
                    )*
                ),*
            ]
        }

        pub fn print_for_matches<'a, 'tcx: 'a>(matches: &ArgMatches, tcx: TyCtxt<'a, 'tcx, 'tcx>, crate_data: &CrateMetadata) {
            let cmd = matches.subcommand_name();
            match cmd.unwrap() {
                $(
                    stringify!($cmd) => concat_idents!(print_, $cmd)(
                        tcx,
                        crate_data,
                        matches.subcommand_matches(stringify!($cmd)).unwrap(),
                    ),
                )*
                _ => unreachable!(),
            }
        }
    }
}

commands! {
    metadata() = "print_metadata";
    lang_items() = "print declared lang items";
    deps() = "print dependencies";
    macros() = "print declared and reexported (proc)macros";
    symbols() = "print exported symbols";
    mir(DEFID) = "print mir for fn of given def_id";
}

fn print_metadata(tcx: TyCtxt, crate_data: &CrateMetadata, _matches: &ArgMatches) {
    macro_rules! print_crateroot {
        ( @one $crate_root:ident bool $field:ident) => {
            svmeta!(@print $field, PrettyBool($crate_root.$field));
        };
        ( @one $crate_root:ident $field:ident) => {
            svmeta!(@print $field, $crate_root.$field);
        };
        ( @print $crate_root:ident $name:expr, $val:expr) => {
            println!("{}{:<30}{}: {:?}", Fg(Cyan), stringify!($name), Fg(Reset), $val);
        };
        ( $crate_root:ident; $( ($($tts:tt)*) )* ) => {
            $(
                print_crateroot!(@one $crate_root $($tts)*);
            )*
        };
    }

    macro_rules! svmeta {
        ( @one $name:ident bool $val:expr) => {
            svmeta!(@print $name, PrettyBool($val));
        };
        ( @one $name:ident $val:expr) => {
            svmeta!(@print $name, $val);
        };
        ( @print $name:expr, $val:expr) => {
            println!("{}{:<30}{}: {:?}", Fg(Cyan), stringify!($name), Fg(Reset), $val);
        };
        ( $( ($($tts:tt)*) )* ) => {
            $(
                svmeta!(@one $($tts)*);
            )*
        };
    }

    // docs at https://doc.rust-lang.org/nightly/nightly-rustc/rustc_metadata/schema/struct.CrateRoot.html
    let crate_root = crate_data.blob.get_root();

    print_crateroot! {
        crate_root;
        (name) (triple) (extra_filename) (hash) (disambiguator) (panic_strategy) (edition)
        (bool has_global_allocator) (bool has_panic_handler) (bool has_default_lib_allocator)
        (plugin_registrar_fn) (proc_macro_decls_static)
        (bool compiler_builtins) (bool needs_allocator) (bool needs_panic_runtime) (bool no_builtins) (bool panic_runtime) (bool profiler_runtime) (bool sanitizer_runtime)
    }

    svmeta! {
        (dep_kind tcx.dep_kind(crate_data.cnum))
    }

    println!("{}crate source{}: {} {} {}",
        Fg(Cyan), Fg(Reset),
        crate_data.source.dylib.as_ref().map(|&(ref p, kind)|format!("{} ({:?})", p.display(), kind)).unwrap_or_else(||String::new()),
        crate_data.source.rlib .as_ref().map(|&(ref p, kind)|format!("{} ({:?})", p.display(), kind)).unwrap_or_else(||String::new()),
        crate_data.source.rmeta.as_ref().map(|&(ref p, kind)|format!("{} ({:?})", p.display(), kind)).unwrap_or_else(||String::new()));
}

fn print_lang_items(tcx: TyCtxt, crate_data: &CrateMetadata, _matches: &ArgMatches) {
    header!("Lang items:");
    for lang_item in crate_data.get_lang_items() {
        out_line!(
            // Double format for the alignment
            format!("{:?}", LangItem::from_u32(lang_item.1 as u32).unwrap()),
            "{}",
            tcx.absolute_item_path_str(lang_item.0),
        );
    }
}

fn print_deps(tcx: TyCtxt, crate_data: &CrateMetadata, _matches: &ArgMatches) {
    header!("Dependent rlibs");
    for dep in crate_data.root.crate_deps.decode(crate_data) {
        out_line!(dep.name, "{:?} ({})", dep.kind, dep.hash);
    }

    header!("Native libraries:");
    for native_lib in crate_data.get_native_libraries(tcx.sess) {
        /*out_line!(native_lib.name,
            "{}",
            match native_lib.kind {
                NativeLibraryKind::NativeStatic => "static lib",
                NativeLibraryKind::NativeStaticNobundle => "static lib, not bundled",
                NativeLibraryKind::NativeFramework => "mac os framework",
                NativeLibraryKind::NativeUnknown => "<unknown>",
            }
        );*/
    }

    header!("Dylib dependency formats:");
    for dylib_deps in crate_data.get_dylib_dependency_formats() {
        let ext_crate_data = tcx.crate_data_as_rc_any(dylib_deps.0);
        let ext_crate_data = ext_crate_data.downcast_ref::<CrateMetadata>().unwrap();
        out_line!(ext_crate_data.name, "{:?}", dylib_deps.1);
    }
}

fn print_macros(tcx: TyCtxt, crate_data: &CrateMetadata, _matches: &ArgMatches) {
    header!("Proc macros:");
    for &(ref proc_macro_name, ref syntax_ext) in crate_data.proc_macros.as_ref().map(|m|&**m).unwrap_or(&[]) {
        out_line!(proc_macro_name, "{:?}", syntax_ext.kind());
    }

    header!("Macros:");
    for_each_export(tcx, &crate_data, tcx.sess, &|export| {
        use syntax::ext::base::MacroKind;
        match export.def {
            Def::Macro(_def_id, macro_kind) => {
                Some(match macro_kind {
                    MacroKind::Bang => "macro!",
                    MacroKind::Attr => "#[macro]",
                    MacroKind::Derive => "#[derive(Macro)]",
                    MacroKind::ProcMacroStub => "<proc-macro-stub>",
                })
            }
            _ => None
        }
    });
}

fn print_symbols<'a, 'tcx: 'a>(tcx: TyCtxt<'a, 'tcx, 'tcx>, crate_data: &CrateMetadata, _matches: &ArgMatches) {
    header!("Exported symbols:");
    if crate_data.proc_macros.is_none() {
        for (exported_symbol, export_level) in crate_data.exported_symbols(tcx).into_iter().take(50) {
            match exported_symbol {
                ExportedSymbol::NonGeneric(def_id) => println!("    {:>4?} {} ({:?})", export_level, tcx.absolute_item_path_str(def_id), def_id),
                ExportedSymbol::Generic(def_id, substs) => println!("    {:>4?} {} ({:?}<{:?}>)", export_level, tcx.absolute_item_path_str(def_id), def_id, substs),
                ExportedSymbol::NoDefId(symbol_name) => println!("    {:>4?} <no def_id> ({:?})", export_level, symbol_name),
            }
        }
    } else {
        // FIXME: support it
        println!("Sorry exported symbols reading not supported for proc macros at the moment :(");
    }

    header!("Trait impls:");
    let mut trait_impls = Vec::new();
    crate_data.get_implementations_for_trait(None, &mut trait_impls);
    for def_id in trait_impls.into_iter().take(50) {
        println!("    {}", tcx.absolute_item_path_str(def_id));
    }

    header!("Types:");
    for_each_export(tcx, &crate_data, tcx.sess, &|export| {
        match export.def {
            Def::Struct(_) |
            Def::Union(_) |
            Def::Enum(_) |
            Def::Trait(_) |
            Def::ForeignTy(_) => Some(export.def.kind_name()),
            Def::TyAlias(_) => Some("type"),
            _ => None
        }
    });

    header!("Items:");
    for_each_export(tcx, &crate_data, tcx.sess, &|export| {
        match export.def {
            Def::Fn(_) |
            Def::Struct(_) |
            Def::Union(_) |
            Def::Enum(_) |
            Def::Trait(_) |
            Def::TyAlias(_) |
            Def::ForeignTy(_) |
            Def::Macro(_, _) => None, // Already handled
            Def::StructCtor(_, _) |
            Def::Variant(_) |
            Def::VariantCtor(_, _) => None, // Not very useful
            Def::Const(_) => Some("const"),
            Def::Static(_, _) => Some("static"),
            _ => Some(export.def.kind_name()),
        }
    });
}

fn print_mir(tcx: TyCtxt, _crate_data: &CrateMetadata, matches: &ArgMatches) {
    let def_id = matches.value_of("DEFID").unwrap();
    let def_id = parse_defid_from_str(def_id);
    println!("mir for {}:\n", tcx.absolute_item_path_str(def_id));
    let mut mir = ::std::io::Cursor::new(Vec::new());
    ::rustc_mir::util::write_mir_pretty(tcx, Some(def_id), &mut mir).unwrap();
    let mir = String::from_utf8(mir.into_inner()).unwrap();
    println!("{}", mir);
}

fn for_each_export<F: Fn(Export) -> Option<&'static str>>(tcx: TyCtxt, crate_data: &CrateMetadata, sess: &Session, callback: &F) {
    use rustc::hir::def_id::DefIndex;
    fn each_export_inner<F: Fn(Export) -> Option<&'static str>>(tcx: TyCtxt, crate_data: &CrateMetadata, id: DefIndex, callback: &F, sess: &Session) {
        crate_data.each_child_of_item(id, |e| {
            match e.def {
                Def::Mod(def_id) => {
                    //println!("mod {}", tcx.absolute_item_path_str(def_id));
                    each_export_inner(tcx, crate_data, def_id.index, callback, sess);
                },
                _ => {
                    if let Some(name) = callback(e) {
                        println!("    {}{:<10}{} {}", Fg(Cyan), name, Fg(Reset), tcx.absolute_item_path_str(e.def.def_id()));
                    }
                },
            }
        }, sess);
    }
    each_export_inner(tcx, crate_data, ::rustc::hir::def_id::CRATE_DEF_INDEX, callback, sess);
}

/*fn for_each_export<F: Fn(Export) -> Option<&'static str>>(tcx: TyCtxt, crate_data: &CrateMetadata, sess: &Session, callback: &F) {
    use rustc_metadata::decoder::Metadata;
    let entries = ::std::collections::HashSet::new();
    let todo = Vec::new();
    todo.push(crate_data.root.index.lookup(crate_data.blob.raw_bytes(), ::rustc::hir::def_id::CRATE_DEF_INDEX).unwrap());
    loop {
        if let Some(entry) = todo.pop() {
            if let Some(children) = entry.children {
                for child in children {

                }
            }
        } else {
            break;
        }
    }
}*/
fn parse_defid_from_str(s: &str) -> DefId {
    let regex = Regex::new(r#"(\d+)/(0|1):(\d+)"#).unwrap();
    // 1/0:14824
    // ^ ^ ^
    // | | DefIndex::as_array_index()
    // | DefIndexAddressSpace
    // CrateNum
    let caps = regex.captures(s).expect("Invalid DefId");
    let crate_num = CrateNum::new(caps.get(1).unwrap().as_str().parse::<usize>().unwrap());
    let address_space = match caps.get(2).unwrap().as_str().parse::<u8>().unwrap() {
        0 => DefIndexAddressSpace::Low,
        1 => DefIndexAddressSpace::High,
        _ => unreachable!(),
    };
    let index = caps.get(3).unwrap().as_str().parse::<usize>().unwrap();
    let def_index = DefIndex::from_array_index(index, address_space);
    DefId {
        krate: crate_num,
        index: def_index,
    }
}
