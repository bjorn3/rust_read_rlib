#![feature(rustc_private, concat_idents)]

extern crate getopts;
extern crate owning_ref;
extern crate flate2;
extern crate termion;
extern crate clap;
extern crate regex;

extern crate syntax;
extern crate rustc_data_structures;

extern crate rustc;
extern crate rustc_back;
extern crate rustc_errors;
extern crate rustc_metadata;
extern crate rustc_incremental;
extern crate rustc_driver;
extern crate rustc_trans_utils;
extern crate rustc_mir;

use std::rc::Rc;

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
        //let args = ::std::env::args().collect::<Vec<_>>();
        let args = vec![rlib.clone(), "/some_nonexistent_dummy_path".to_string()];
        driver::call_with_crate_tcx(args, &rlib, Rc::new(move |tcx, metadata| {
            print::print_for_matches(&matches, tcx, metadata);
        }));
    }).unwrap().join();
}
