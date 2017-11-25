#![feature(rustc_private)]

extern crate getopts;
extern crate owning_ref;
extern crate flate2;

extern crate syntax;
extern crate rustc_data_structures;

extern crate rustc;
extern crate rustc_back;
extern crate rustc_errors;
extern crate rustc_metadata;
extern crate rustc_incremental;
extern crate rustc_trans;
extern crate rustc_driver;
extern crate rustc_trans_utils;

use std::path::{Path, PathBuf};

use rustc::ty::TyCtxt;
use rustc::session::Session;
use rustc_metadata::cstore::CrateMetadata;
use rustc_driver::{Compilation, RustcDefaultCalls};
use rustc_driver::driver::CompileController;

fn find_sysroot() -> String {
    if let Ok(sysroot) = std::env::var("MIRI_SYSROOT") {
        return sysroot;
    }

    // Taken from https://github.com/Manishearth/rust-clippy/pull/911.
    let home = option_env!("RUSTUP_HOME").or(option_env!("MULTIRUST_HOME"));
    let toolchain = option_env!("RUSTUP_TOOLCHAIN").or(option_env!("MULTIRUST_TOOLCHAIN"));
    match (home, toolchain) {
        (Some(home), Some(toolchain)) => format!("{}/toolchains/{}", home, toolchain),
        _ => {
            option_env!("RUST_SYSROOT")
                .expect(
                    "need to specify RUST_SYSROOT env var or use rustup or multirust",
                )
                .to_owned()
        }
    }
}

fn main() {
    let rlib = ::std::env::args().skip(1).next().expect("No rlib given");
    println!("Reading rlib {}", rlib);

    let mut args = ::std::env::args().collect::<Vec<_>>();
    let sysroot_flag = String::from("--sysroot");
    if !args.contains(&sysroot_flag) {
        args.push(sysroot_flag);
        args.push(find_sysroot());
    }
    println!("Rust args: {:?}", args);
    rustc_driver::run_compiler(
        &args,
        &mut MyCompilerCalls(RustcDefaultCalls, rlib.to_string()),
        Some(Box::new(MyFileLoader(rlib.to_string())) as Box<_>),
        None
    );
}

struct MyFileLoader(String);

impl syntax::codemap::FileLoader for MyFileLoader {
    fn file_exists(&self, _: &Path) -> bool { true }
    fn abs_path(&self, _: &Path) -> Option<PathBuf> { None }
    fn read_file(&self, _: &Path) -> Result<String, ::std::io::Error> {
        Ok(format!("
        #![feature(no_core)]
        #![feature(rustc_private)]
        #![feature(panic_unwind)]
        #![feature(panic_abort)]
        #![feature(compiler_builtins_lib)]
        #![no_core]
        extern crate {};
        fn main() {{}}", self.0))
    }
}

struct MyCompilerCalls(RustcDefaultCalls, String);

impl<'a> rustc_driver::CompilerCalls<'a> for MyCompilerCalls {
    fn build_controller(
        &mut self,
        sess: &Session,
        matches: &getopts::Matches
    ) -> CompileController<'a> {
        let mut control = self.0.build_controller(sess, matches);
        control.after_analysis.stop = Compilation::Stop;
        control.after_analysis.callback = Box::new(|state|{
            let tcx = state.tcx.as_ref().unwrap();
            let mut extern_crate = None;
            for item in tcx.hir.krate().items.values() {
                match item.node {
                    ::rustc::hir::Item_::ItemExternCrate(_) => {
                        extern_crate = Some(item.id);
                        break;
                    }
                    _ => {}
                }
            }
            let ext_cnum = tcx.extern_mod_stmt_cnum(tcx.hir.local_def_id(extern_crate.unwrap())).unwrap();
            let crate_data = tcx.crate_data_as_rc_any(ext_cnum);
            let crate_data = crate_data.downcast_ref::<CrateMetadata>().unwrap();
            print_metadata(*tcx, crate_data);
        });
        control
    }
}

macro_rules! language_item_table {
    ( $( $variant:ident,$name:expr,$_method:ident; )* ) => {
        fn lang_item_name(lang_item: ::rustc::middle::lang_items::LangItem) -> &'static str {
            use rustc::middle::lang_items::LangItem;
            match lang_item {
                $(
                    LangItem::$variant => $name
                ),*
            }
        }
    };
}

// Copied from librustc/middle/lang_items.rs
language_item_table! {
//  Variant name,                    Name,                      Method name;
    CharImplItem,                    "char",                    char_impl;
    StrImplItem,                     "str",                     str_impl;
    SliceImplItem,                   "slice",                   slice_impl;
    SliceU8ImplItem,                 "slice_u8",                slice_u8_impl;
    ConstPtrImplItem,                "const_ptr",               const_ptr_impl;
    MutPtrImplItem,                  "mut_ptr",                 mut_ptr_impl;
    I8ImplItem,                      "i8",                      i8_impl;
    I16ImplItem,                     "i16",                     i16_impl;
    I32ImplItem,                     "i32",                     i32_impl;
    I64ImplItem,                     "i64",                     i64_impl;
    I128ImplItem,                     "i128",                   i128_impl;
    IsizeImplItem,                   "isize",                   isize_impl;
    U8ImplItem,                      "u8",                      u8_impl;
    U16ImplItem,                     "u16",                     u16_impl;
    U32ImplItem,                     "u32",                     u32_impl;
    U64ImplItem,                     "u64",                     u64_impl;
    U128ImplItem,                    "u128",                    u128_impl;
    UsizeImplItem,                   "usize",                   usize_impl;
    F32ImplItem,                     "f32",                     f32_impl;
    F64ImplItem,                     "f64",                     f64_impl;

    SizedTraitLangItem,              "sized",                   sized_trait;
    UnsizeTraitLangItem,             "unsize",                  unsize_trait;
    CopyTraitLangItem,               "copy",                    copy_trait;
    CloneTraitLangItem,              "clone",                   clone_trait;
    SyncTraitLangItem,               "sync",                    sync_trait;
    FreezeTraitLangItem,             "freeze",                  freeze_trait;

    DropTraitLangItem,               "drop",                    drop_trait;

    CoerceUnsizedTraitLangItem,      "coerce_unsized",          coerce_unsized_trait;

    AddTraitLangItem,                "add",                     add_trait;
    SubTraitLangItem,                "sub",                     sub_trait;
    MulTraitLangItem,                "mul",                     mul_trait;
    DivTraitLangItem,                "div",                     div_trait;
    RemTraitLangItem,                "rem",                     rem_trait;
    NegTraitLangItem,                "neg",                     neg_trait;
    NotTraitLangItem,                "not",                     not_trait;
    BitXorTraitLangItem,             "bitxor",                  bitxor_trait;
    BitAndTraitLangItem,             "bitand",                  bitand_trait;
    BitOrTraitLangItem,              "bitor",                   bitor_trait;
    ShlTraitLangItem,                "shl",                     shl_trait;
    ShrTraitLangItem,                "shr",                     shr_trait;
    AddAssignTraitLangItem,          "add_assign",              add_assign_trait;
    SubAssignTraitLangItem,          "sub_assign",              sub_assign_trait;
    MulAssignTraitLangItem,          "mul_assign",              mul_assign_trait;
    DivAssignTraitLangItem,          "div_assign",              div_assign_trait;
    RemAssignTraitLangItem,          "rem_assign",              rem_assign_trait;
    BitXorAssignTraitLangItem,       "bitxor_assign",           bitxor_assign_trait;
    BitAndAssignTraitLangItem,       "bitand_assign",           bitand_assign_trait;
    BitOrAssignTraitLangItem,        "bitor_assign",            bitor_assign_trait;
    ShlAssignTraitLangItem,          "shl_assign",              shl_assign_trait;
    ShrAssignTraitLangItem,          "shr_assign",              shr_assign_trait;
    IndexTraitLangItem,              "index",                   index_trait;
    IndexMutTraitLangItem,           "index_mut",               index_mut_trait;

    UnsafeCellTypeLangItem,          "unsafe_cell",             unsafe_cell_type;

    DerefTraitLangItem,              "deref",                   deref_trait;
    DerefMutTraitLangItem,           "deref_mut",               deref_mut_trait;

    FnTraitLangItem,                 "fn",                      fn_trait;
    FnMutTraitLangItem,              "fn_mut",                  fn_mut_trait;
    FnOnceTraitLangItem,             "fn_once",                 fn_once_trait;

    GeneratorStateLangItem,          "generator_state",         gen_state;
    GeneratorTraitLangItem,          "generator",               gen_trait;

    EqTraitLangItem,                 "eq",                      eq_trait;
    OrdTraitLangItem,                "ord",                     ord_trait;

    // A number of panic-related lang items. The `panic` item corresponds to
    // divide-by-zero and various panic cases with `match`. The
    // `panic_bounds_check` item is for indexing arrays.
    //
    // The `begin_unwind` lang item has a predefined symbol name and is sort of
    // a "weak lang item" in the sense that a crate is not required to have it
    // defined to use it, but a final product is required to define it
    // somewhere. Additionally, there are restrictions on crates that use a weak
    // lang item, but do not have it defined.
    PanicFnLangItem,                 "panic",                   panic_fn;
    PanicBoundsCheckFnLangItem,      "panic_bounds_check",      panic_bounds_check_fn;
    PanicFmtLangItem,                "panic_fmt",               panic_fmt;

    ExchangeMallocFnLangItem,        "exchange_malloc",         exchange_malloc_fn;
    BoxFreeFnLangItem,               "box_free",                box_free_fn;
    DropInPlaceFnLangItem,             "drop_in_place",           drop_in_place_fn;

    StartFnLangItem,                 "start",                   start_fn;

    EhPersonalityLangItem,           "eh_personality",          eh_personality;
    EhUnwindResumeLangItem,          "eh_unwind_resume",        eh_unwind_resume;
    MSVCTryFilterLangItem,           "msvc_try_filter",         msvc_try_filter;

    OwnedBoxLangItem,                "owned_box",               owned_box;

    PhantomDataItem,                 "phantom_data",            phantom_data;

    NonZeroItem,                     "non_zero",                non_zero;

    DebugTraitLangItem,              "debug_trait",             debug_trait;

    // A lang item for each of the 128-bit operators we can optionally lower.
    I128AddFnLangItem,               "i128_add",                i128_add_fn;
    U128AddFnLangItem,               "u128_add",                u128_add_fn;
    I128SubFnLangItem,               "i128_sub",                i128_sub_fn;
    U128SubFnLangItem,               "u128_sub",                u128_sub_fn;
    I128MulFnLangItem,               "i128_mul",                i128_mul_fn;
    U128MulFnLangItem,               "u128_mul",                u128_mul_fn;
    I128DivFnLangItem,               "i128_div",                i128_div_fn;
    U128DivFnLangItem,               "u128_div",                u128_div_fn;
    I128RemFnLangItem,               "i128_rem",                i128_rem_fn;
    U128RemFnLangItem,               "u128_rem",                u128_rem_fn;
    I128ShlFnLangItem,               "i128_shl",                i128_shl_fn;
    U128ShlFnLangItem,               "u128_shl",                u128_shl_fn;
    I128ShrFnLangItem,               "i128_shr",                i128_shr_fn;
    U128ShrFnLangItem,               "u128_shr",                u128_shr_fn;
    // And overflow versions for the operators that are checkable.
    // While MIR calls these Checked*, they return (T,bool), not Option<T>.
    I128AddoFnLangItem,              "i128_addo",               i128_addo_fn;
    U128AddoFnLangItem,              "u128_addo",               u128_addo_fn;
    I128SuboFnLangItem,              "i128_subo",               i128_subo_fn;
    U128SuboFnLangItem,              "u128_subo",               u128_subo_fn;
    I128MuloFnLangItem,              "i128_mulo",               i128_mulo_fn;
    U128MuloFnLangItem,              "u128_mulo",               u128_mulo_fn;
    I128ShloFnLangItem,              "i128_shlo",               i128_shlo_fn;
    U128ShloFnLangItem,              "u128_shlo",               u128_shlo_fn;
    I128ShroFnLangItem,              "i128_shro",               i128_shro_fn;
    U128ShroFnLangItem,              "u128_shro",               u128_shro_fn;
}

fn print_metadata(tcx: TyCtxt, crate_data: &CrateMetadata) {
    use rustc::middle::lang_items::LangItem;
    use rustc_metadata::cstore::NativeLibraryKind;

    macro_rules! svmeta {
        ( @one $crate_data:expr; if $name:ident) => {
            if $crate_data.$name() {
                svmeta!(@print $name, $crate_data.$name());
            }
        };
        ( @one $crate_data:expr; if $name:ident ($arg:expr)) => {
            if $crate_data.$name($arg) {
                svmeta!(@print $name, $crate_data.$name($arg));
            }
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
            let name = stringify!($name);
            println!("{:<30}: {}", name, $val);
        };
        ( $crate_data:expr; $( ($($tts:tt)*) )* ) => {
            $(
                svmeta!(@one $crate_data; $($tts)*);
            )*
        };
    }
    //println!("Name: {}", crate_data.name());
    //println!("Hash: {}", crate_data.hash());
    //println!("Disambiguator: {}", crate_data.disambiguator().to_fingerprint());
    svmeta! {
        crate_data;
        (name)
        (hash)
        (disambiguator.to_fingerprint)
        (needs_allocator(tcx.sess))
        (if has_global_allocator)
        (if has_default_lib_allocator)
        (if is_panic_runtime(tcx.sess))
        (if is_compiler_builtins(tcx.sess))
        (if is_sanitizer_runtime(tcx.sess))
        (if is_profiler_runtime(tcx.sess))
        (if is_no_builtins(tcx.sess))
        (if has_copy_closures(tcx.sess))
        (if has_clone_closures(tcx.sess))
        (panic_strategy.desc)
    }

    println!("\nLang items:");
    for lang_item in crate_data.get_lang_items() {
        println!("    {}: {}",
            lang_item_name(LangItem::from_u32(lang_item.1 as u32).unwrap()),
            tcx.absolute_item_path_str(lang_item.0),
        );
    }

    println!("\nExported symbols:");
    for def_id in crate_data.get_exported_symbols() {
        println!("    {}", tcx.absolute_item_path_str(def_id));
    }

    println!("\nTrait impls:");
    let mut trait_impls = Vec::new();
    crate_data.get_implementations_for_trait(None, &mut trait_impls);
    for def_id in trait_impls {
        println!("    {}", tcx.absolute_item_path_str(def_id));
    }

    println!("\nDependent rlibs");
    for dep in crate_data.root.crate_deps.decode(crate_data) {
        println!("    {:<26}: {:?} ({})", dep.name, dep.kind, dep.hash);
    }

    println!("\nNative libraries:");
    for native_lib in crate_data.get_native_libraries(tcx.sess) {
        println!("    {:<26}: {}",
            native_lib.name,
            match native_lib.kind {
                NativeLibraryKind::NativeStatic => "static lib",
                NativeLibraryKind::NativeStaticNobundle => "static lib, not bundled",
                NativeLibraryKind::NativeFramework => "mac os framework",
                NativeLibraryKind::NativeUnknown => "<unknown>",
            }
        );
    }

    println!("\nDylib dependency formats:");
    for dylib_deps in crate_data.get_dylib_dependency_formats() {
        let ext_crate_data = tcx.crate_data_as_rc_any(dylib_deps.0);
        let ext_crate_data = ext_crate_data.downcast_ref::<CrateMetadata>().unwrap();
        println!("    {:<26}: {:?}", ext_crate_data.name(), dylib_deps.1);
    }
}
