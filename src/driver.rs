use std::rc::Rc;
use std::path::{Path, PathBuf};

use rustc::session::Session;
use rustc::ty::TyCtxt;

use rustc_metadata::cstore::CrateMetadata;
use rustc_driver::{Compilation, RustcDefaultCalls};
use rustc_driver::driver::CompileController;

fn find_sysroot() -> String {
    if let Ok(sysroot) = ::std::env::var("MIRI_SYSROOT") {
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

pub fn call_with_crate_tcx(mut args: Vec<String>, rlib: &str, f: Rc<Fn(TyCtxt, &CrateMetadata)>) {
    println!("Reading rlib {}", rlib);

    let sysroot_flag = String::from("--sysroot");
    if !args.contains(&sysroot_flag) {
        args.push(sysroot_flag);
        args.push(find_sysroot());
    }
    println!("Rust args: {:?}", args);
    ::rustc_driver::run_compiler(
        &args,
        &mut MyCompilerCalls(RustcDefaultCalls, rlib.to_string(), f),
        Some(Box::new(MyFileLoader(rlib.to_string())) as Box<_>),
        None
    );
}

struct MyFileLoader(String);

impl ::syntax::codemap::FileLoader for MyFileLoader {
    fn file_exists(&self, _: &Path) -> bool { true }
    fn abs_path(&self, _: &Path) -> Option<PathBuf> { None }
    fn read_file(&self, _: &Path) -> Result<String, ::std::io::Error> {
        Ok(format!("
        #![feature(no_core)]
        #![feature(rustc_private)]
        #![feature(panic_unwind)]
        #![feature(panic_abort)]
        #![feature(compiler_builtins_lib)]
        #![allow(unused_imports)]
        #![no_std] // Replace with `#![no_core]` for reading libcore metadata
        extern crate {} as __dummy_name;
        fn main() {{}}", self.0))
    }
}

struct MyCompilerCalls(RustcDefaultCalls, String, Rc<Fn(TyCtxt, &CrateMetadata)>);

impl<'a> ::rustc_driver::CompilerCalls<'a> for MyCompilerCalls {
    fn build_controller(
        &mut self,
        sess: &Session,
        matches: &::getopts::Matches
    ) -> CompileController<'a> {
        let mut control = self.0.build_controller(sess, matches);
        control.after_analysis.stop = Compilation::Stop;
        let cb = self.2.clone();
        control.after_analysis.callback = Box::new(move |state|{
            let tcx = state.tcx.as_ref().unwrap();
            let mut extern_crate = None;
            for item in tcx.hir.krate().items.values() {
                match item.node {
                    ::rustc::hir::Item_::ItemExternCrate(_) => {
                        extern_crate = Some(item.id);
                        // Continue iterating to get the last `extern crate`
                    }
                    _ => {}
                }
            }
            let ext_cnum = tcx.extern_mod_stmt_cnum(tcx.hir.local_def_id(extern_crate.unwrap())).unwrap();
            let crate_data = tcx.crate_data_as_rc_any(ext_cnum);
            let crate_data = crate_data.downcast_ref::<CrateMetadata>().unwrap();
            cb(*tcx, crate_data);
        });
        control
    }
}
