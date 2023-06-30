#![feature(rustc_private, concat_idents)]

extern crate termion;
extern crate clap;
extern crate regex;

extern crate rustc_data_structures;

extern crate rustc_middle as rustc;
extern crate rustc_metadata;
extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_session;
extern crate rustc_hir;
extern crate rustc_span;
extern crate rustc_expand;

use clap::{Arg, App};

mod driver;
mod print;

fn main() {
    let _ = ::std::thread::Builder::new()
        .name("new main thread".to_string())
        .stack_size(1024*1024*1024)
        .spawn(||{
        let matches = App::new("read rlib")
            .version("0.1")
            .author("bjorn3")
            .about("Read a rlib created by rustc")
            .arg(Arg::with_name("CRATE")
                .help("The crate name")
                .required(true)
                .index(1))
            .subcommands(print::subcommands())
            .get_matches();
        let rlib = matches.value_of("CRATE").unwrap().to_string();
        let args = vec![rlib.clone(), "/some_nonexistent_dummy_path".to_string()];
        driver::call_with_crate_tcx(args, rlib, Box::new(move |tcx| {
            driver::with_crate_metadata(tcx, |metadata| {
                print::print_for_matches(&matches, tcx, metadata);
            });
        }));
    }).unwrap().join();
}
