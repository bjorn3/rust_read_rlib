#![feature(rustc_private)]

extern crate getopts;
extern crate owning_ref;
extern crate flate2;
extern crate termion;

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

use std::rc::Rc;

mod driver;
mod print;

fn main() {
    let rlib = ::std::env::args().skip(1).next().expect("No rlib given");
    let args = ::std::env::args().collect::<Vec<_>>();
    driver::call_with_crate_tcx(args, &rlib, Rc::new(|tcx, metadata| print::print_metadata(tcx, metadata)));
}
