//! The command line tool `cdx` is a set of tools for text file manipulation
//! and command line data mining.
//! It is hoped that the associated library will be useful for theid party tools.

#![warn(
    absolute_paths_not_starting_with_crate,
    explicit_outlives_requirements,
    keyword_idents,
    noop_method_call,
    rust_2021_incompatible_closure_captures,
    rust_2021_incompatible_or_patterns,
    rust_2021_prefixes_incompatible_syntax,
    rust_2021_prelude_collisions,
    missing_debug_implementations,
    missing_docs,
    rust_2018_idioms,
    trivial_numeric_casts,
    trivial_casts,
    unreachable_pub,
    unused_lifetimes,
    unused_extern_crates,
    unused_qualifications,

//    clippy::all,
//    clippy::restriction,
//    clippy::pedantic,
    clippy::nursery,
    clippy::cargo,
)]
#![allow(clippy::multiple_crate_versions)]
#![allow(clippy::redundant_pub_crate)]
#![allow(clippy::option_if_let_else)]

pub mod agg;
pub mod binsearch;
pub mod column;
pub mod comp;
pub mod expr;
pub mod join;
pub mod matcher;
pub mod num;
pub mod sampler;
mod shunting_yard;
pub mod sort;
pub mod tabs;
pub mod text;
pub mod textgen;
//mod tokenizer;
mod tok2;
pub mod tooltest;
pub mod trans;
pub mod transpose;
pub mod util;
