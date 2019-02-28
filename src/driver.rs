use std::path::{Path, PathBuf};

use rustc::session::Session;
use rustc::ty::TyCtxt;

use rustc_metadata::cstore::CrateMetadata;
use rustc_driver::{Compilation, CompilerCalls, RustcDefaultCalls};
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

pub fn call_with_crate_tcx(mut args: Vec<String>, rlib: String, f: Box<Cb>) {
    println!("Reading rlib {}", rlib);

    let sysroot_flag = String::from("--sysroot");
    if !args.contains(&sysroot_flag) {
        args.push(sysroot_flag);
        args.push(find_sysroot());
    }

    args.push("-Cpanic=abort".to_string()); // otherwise we have to provide `eh_personality`

    println!("Rust args: {:?}", args);
    let result = rustc_driver::run(move || {
        ::rustc_driver::run_compiler(
            &args,
            Box::new(MyCompilerCalls(RustcDefaultCalls, rlib.to_string(), f)),
            Some(Box::new(MyFileLoader(rlib.to_string())) as Box<_>),
            None
        )
    });
    ::std::process::exit(result as i32);
}

struct MyFileLoader(String);

impl ::syntax::source_map::FileLoader for MyFileLoader {
    fn file_exists(&self, _: &Path) -> bool { true }
    fn abs_path(&self, _: &Path) -> Option<PathBuf> { None }
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

type Cb = for<'a, 'tcx> Fn(TyCtxt<'a, 'tcx, 'tcx>) + Send;

struct MyCompilerCalls(RustcDefaultCalls, String, Box<Cb>);

impl<'a> CompilerCalls<'a> for MyCompilerCalls {
    fn build_controller(
        self: Box<Self>,
        sess: &Session,
        matches: &::getopts::Matches
    ) -> CompileController<'a> {
        let mut control = Box::new(self.0).build_controller(sess, matches);
        control.after_analysis.stop = Compilation::Stop;
        control.after_analysis.callback = Box::new(move |state|{
            let tcx = state.tcx.as_ref().unwrap();
            (self.2)(*tcx);
        });
        control
    }
}

pub fn with_crate_metadata<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>, f: impl for<'b> FnOnce(&'b CrateMetadata)) {
    let mut extern_crate = None;
    for item in tcx.hir().krate().items.values() {
        match item.node {
            ::rustc::hir::ItemKind::ExternCrate(_) => {
                extern_crate = Some(item.id);
                // Continue iterating to get the last `extern crate`
            }
            _ => {}
        }
    }
    let ext_cnum = tcx.extern_mod_stmt_cnum(tcx.hir().local_def_id(extern_crate.unwrap())).unwrap();
    let crate_data = tcx.crate_data_as_rc_any(ext_cnum);
    f(crate_data.downcast_ref::<CrateMetadata>().unwrap());
}
