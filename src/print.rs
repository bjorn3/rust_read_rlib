use std::fmt;

use rustc::ty::TyCtxt;
use rustc::session::Session;
use rustc::hir::def::{Export, Def};
use rustc_metadata::cstore::CrateMetadata;

use termion::color::*;

struct PrettyBool(bool);

impl fmt::Display for PrettyBool {
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

macro_rules! line {
    ($name:expr, $fmt:expr, $($e:tt)*) => {
        println!(concat!("    {}{:<26}{}: ", $fmt), Fg(Cyan), $name, Fg(Reset), $($e)*);
    };
}

macro_rules! svmeta {
    ( @one $crate_data:expr; bool $name:ident) => {
        svmeta!(@print $name, PrettyBool($crate_data.$name()));
    };
    ( @one $crate_data:expr; bool $name:ident ($arg:expr)) => {
        svmeta!(@print $name, PrettyBool($crate_data.$name($arg)));
    };
    ( @one $crate_data:expr; $name:ident) => {
        svmeta!(@print $name, $crate_data.$name());
    };
    ( @one $crate_data:expr; $name:ident ($arg:expr)) => {
        svmeta!(@print $name, $crate_data.$name($arg));
    };
    ( @one $crate_data:expr; $name:ident . $call:ident) => {
        svmeta!(@print $name, $crate_data.$name().$call());
    };
    ( @print $name:ident, $val:expr) => {
        println!("{}{:<30}{}: {}", Fg(Cyan), stringify!($name), Fg(Reset), $val);
    };
    ( $crate_data:expr; $( ($($tts:tt)*) )* ) => {
        $(
            svmeta!(@one $crate_data; $($tts)*);
        )*
    };
}

pub fn print_metadata(tcx: TyCtxt, crate_data: &CrateMetadata) {
    use rustc::middle::lang_items::LangItem;
    use rustc_metadata::cstore::NativeLibraryKind;

    svmeta! {
        crate_data;
        (name)
        (hash)
        (disambiguator.to_fingerprint)
        (bool needs_allocator(tcx.sess))
        (bool has_global_allocator)
        (bool has_default_lib_allocator)
        (bool is_panic_runtime(tcx.sess))
        (bool needs_panic_runtime(tcx.sess))
        (bool is_compiler_builtins(tcx.sess))
        (bool is_sanitizer_runtime(tcx.sess))
        (bool is_profiler_runtime(tcx.sess))
        (bool is_no_builtins(tcx.sess))
        (bool has_copy_closures(tcx.sess))
        (bool has_clone_closures(tcx.sess))
        (panic_strategy.desc)
    }

    println!("{}crate source{}: {} {} {}",
        Fg(Cyan), Fg(Reset),
        crate_data.source.dylib.as_ref().map(|&(ref p, kind)|format!("{} ({:?})", p.display(), kind)).unwrap_or_else(||String::new()),
        crate_data.source.rlib .as_ref().map(|&(ref p, kind)|format!("{} ({:?})", p.display(), kind)).unwrap_or_else(||String::new()),
        crate_data.source.rmeta.as_ref().map(|&(ref p, kind)|format!("{} ({:?})", p.display(), kind)).unwrap_or_else(||String::new()));

    header!("Lang items:");
    for lang_item in crate_data.get_lang_items() {
        line!(
            // Double format for the alignment
            format!("{:?}", LangItem::from_u32(lang_item.1 as u32).unwrap()),
            "{}",
            tcx.absolute_item_path_str(lang_item.0),
        );
    }

    header!("Dependent rlibs");
    for dep in crate_data.root.crate_deps.decode(crate_data) {
        line!(dep.name, "{:?} ({})", dep.kind, dep.hash);
    }

    header!("Native libraries:");
    for native_lib in crate_data.get_native_libraries(tcx.sess) {
        line!(native_lib.name,
            "{}",
            match native_lib.kind {
                NativeLibraryKind::NativeStatic => "static lib",
                NativeLibraryKind::NativeStaticNobundle => "static lib, not bundled",
                NativeLibraryKind::NativeFramework => "mac os framework",
                NativeLibraryKind::NativeUnknown => "<unknown>",
            }
        );
    }

    header!("Dylib dependency formats:");
    for dylib_deps in crate_data.get_dylib_dependency_formats() {
        let ext_crate_data = tcx.crate_data_as_rc_any(dylib_deps.0);
        let ext_crate_data = ext_crate_data.downcast_ref::<CrateMetadata>().unwrap();
        line!(ext_crate_data.name(), "{:?}", dylib_deps.1);
    }

    header!("Proc macros:");
    for &(ref proc_macro_name, ref syntax_ext) in crate_data.proc_macros.as_ref().map(|m|&**m).unwrap_or(&[]) {
        line!(proc_macro_name, "{:?}", syntax_ext.kind());
    }

    header!("Macros:");
    for_each_export(tcx, &crate_data, tcx.sess, &|export| {
        use syntax::ext::base::MacroKind;
        match export.def {
            Def::Macro(_def_id, macro_kind) => {
                Some(match macro_kind {
                    MacroKind::Bang => "macro!",
                    MacroKind::Attr => "#[macro]",
                    MacroKind::Derive => "#[derive(Macro)]"
                })
            }
            _ => None
        }
    });

    header!("Exported symbols:");
    if crate_data.proc_macros.is_none() {
        for def_id in crate_data.get_exported_symbols().into_iter().take(50) {
            println!("    {}", tcx.absolute_item_path_str(def_id));
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
            Def::TyForeign(_) => Some(export.def.kind_name()),
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
            Def::TyForeign(_) |
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

fn for_each_export<F: Fn(Export) -> Option<&'static str>>(tcx: TyCtxt, crate_data: &CrateMetadata, sess: &Session, callback: &F) {
    use rustc::hir::def_id::DefIndex;
    fn each_export_inner<F: Fn(Export) -> Option<&'static str>>(tcx: TyCtxt, crate_data: &CrateMetadata, id: DefIndex, callback: &F, sess: &Session) {
        crate_data.each_child_of_item(id, |e| {
            match e.def {
                Def::Mod(def_id) => each_export_inner(tcx, crate_data, def_id.index, callback, sess),
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