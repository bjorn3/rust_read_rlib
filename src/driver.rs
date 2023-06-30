use std::path::Path;

use rustc::ty::TyCtxt;

use rustc_metadata::rmeta::decoder::CrateMetadata;
use rustc_driver::{Compilation, Callbacks};
use rustc_interface::interface::Compiler;
use rustc_interface::Queries;

pub fn call_with_crate_tcx(mut args: Vec<String>, rlib: String, f: Box<Cb>) -> ! {
    println!("Reading rlib {}", rlib);

    args.push("-Cpanic=abort".to_string()); // otherwise we have to provide `eh_personality`

    println!("Rust args: {:?}", args);
    std::process::exit(rustc_driver::catch_with_exit_code(move || {
        let args: Vec<String> = std::env::args().collect();
        rustc_driver::RunCompiler::new(&args, &mut MyCompilerCalls(f)).run()
    }))
}

struct MyFileLoader(String);

impl ::rustc_span::source_map::FileLoader for MyFileLoader {
    fn file_exists(&self, _: &Path) -> bool { true }
    fn read_binary_file(&self, _: &Path) -> std::io::Result<Vec<u8>> {
        Err(std::io::Error::new(std::io::ErrorKind::Unsupported, "should not need to load binary files"))
    }
    fn read_file(&self, _: &Path) -> Result<String, ::std::io::Error> {
        Ok(format!("
        #![feature(no_core)]
        #![feature(rustc_private)]
        #![feature(compiler_builtins_lib)]
        #![allow(unused_imports)]
        #![no_std] // Replace with `#![no_core]` for reading libcore metadata

        extern crate {} as __{};
        fn main() {{}}", self.0, self.0))
    }
}

type Cb = dyn for<'tcx> Fn(TyCtxt<'tcx>) + Send;

struct MyCompilerCalls(Box<Cb>);

impl Callbacks for MyCompilerCalls {
    fn after_analysis<'tcx>(&mut self, compiler: &Compiler, queries: &'tcx Queries<'tcx>) -> Compilation {
        queries.global_ctxt().unwrap().enter(|tcx| {
            (self.0)(tcx);
        });
        Compilation::Stop
    }

}

pub fn with_crate_metadata<'tcx>(tcx: TyCtxt<'tcx>, f: impl for<'b> FnOnce(&'b CrateMetadata)) {
    let mut extern_crate = None;
    for &item_id in tcx.hir().root_module().item_ids {
        match tcx.hir().item(item_id).kind {
            ::rustc_hir::ItemKind::ExternCrate(_) => {
                extern_crate = Some(item_id.hir_id());
                // Continue iterating to get the last `extern crate`
            }
            _ => {}
        }
    }
    let ext_cnum = tcx.extern_mod_stmt_cnum(extern_crate.unwrap().owner.def_id).unwrap();
    let crate_data = tcx.crate_data_as_rc_any(ext_cnum);
    f(crate_data.downcast_ref::<CrateMetadata>().unwrap());
}
