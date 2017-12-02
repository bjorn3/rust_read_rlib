# Rust read rlib

Read the contents of a rust rlib

## Usage

```bash
$ git clone https://github.com/bjorn3/rust_read_rlib.git && cd rust_read_rlib
$ cargo +nightly run -- std
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/rlib_view std`
Reading rlib std
Rust args: ["target/debug/rlib_view", "std", "--sysroot", "~/.rustup/toolchains/nightly-x86_64-apple-darwin"]
name                          : std
hash                          : a691969a97ae1fb5
disambiguator                 : e05f155455d31103-12fe4bec05f8ff93
needs_allocator               : false
has_default_lib_allocator     : true
panic_strategy                : unwind
crate source: ~/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/x86_64-apple-darwin/lib/libstd-de8d40fc6fd19ec8.dylib (All) ~/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/x86_64-apple-darwin/lib/libstd-de8d40fc6fd19ec8.rlib (All) 

Lang items:
    F32ImplItem: std::f32::<impl f32>
    F64ImplItem: std::f64::<impl f64>
    PanicFmtLangItem: std::panicking::rust_begin_panic
    StartFnLangItem: std::rt::lang_start

Dependent rlibs
    core                      : Explicit (a158defa63941eda)
    alloc                     : Explicit (0a9980dddd11b6aa)
    std_unicode               : Explicit (98272a25ac01c713)
    alloc_system              : Explicit (56bb28dbf722622b)
    libc                      : Explicit (444daafe5aba2099)
    unwind                    : Explicit (82dcf5d4735f0c39)
    compiler_builtins         : Explicit (231fd774e783c37b)
    alloc_jemalloc            : Implicit (8a373e6437a502a4)
    panic_unwind              : Implicit (38ba880588e4fbf7)

Native libraries:
    backtrace                 : static lib
    System                    : <unknown>
    resolv                    : <unknown>

Dylib dependency formats:
    core                      : RequireStatic
    alloc                     : RequireStatic
    std_unicode               : RequireStatic
    alloc_system              : RequireStatic
    libc                      : RequireStatic
    unwind                    : RequireStatic
    compiler_builtins         : RequireStatic
    alloc_jemalloc            : RequireStatic
    panic_unwind              : RequireStatic

Proc macros:

Exported symbols:
    std::ffi::os_str::OsStr::to_str
    <std::io::Error as std::error::Error>::cause
    <std::net::UdpSocket as std::sys_common::IntoInner<std::sys_common::net::UdpSocket>>::into_inner
    std::ffi::c_str::CString::into_bytes_with_nul
    std::fs::File::sync_all
[...]
```
