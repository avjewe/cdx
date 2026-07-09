//! The command line tool `cdx` is a set of tools for text file manipulation
//! and command line data mining.
//! It is hoped that the associated library will be useful for third party tools.

#![warn(
    rustdoc::all,
    absolute_paths_not_starting_with_crate,
    deprecated_in_future,
    explicit_outlives_requirements,
    future_incompatible,
    keyword_idents,
    impl_trait_redundant_captures,
    keyword_idents_2024,
    let_underscore,
    macro_use_extern_crate,
    missing_debug_implementations,
    missing_copy_implementations,
    missing_docs,
    non_ascii_idents,
    // non_exhaustive_omitted_patterns, unstable
    noop_method_call,
    redundant_imports,
    redundant_lifetimes,
    rust_2018_compatibility,
    rust_2021_compatibility,
    rust_2024_compatibility,
    rust_2018_idioms,
    single_use_lifetimes,
    trivial_numeric_casts,
    trivial_casts,
    unit_bindings,
    unreachable_pub,
    unsafe_op_in_unsafe_fn,
    unused,
    unused_crate_dependencies,
    unused_extern_crates,
    unused_lifetimes,
    unused_qualifications,

    clippy::style,
    clippy::pedantic,
    clippy::nursery,
    clippy::cargo,

    // clippy::restriction,
    clippy::create_dir,
    // clippy::exhaustive_enums,
    // clippy::exhaustive_structs,
    clippy::little_endian_bytes,
    clippy::missing_asserts_for_indexing,
    clippy::mixed_read_write_in_expression,
    // clippy::panic,
    clippy::partial_pub_fields,
    clippy::redundant_type_annotations,
    clippy::renamed_function_params,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_name_method,
    clippy::string_add,
    clippy::string_lit_chars_any,
    clippy::tests_outside_test_module,

    // clippy::indexing_slicing,
    // clippy::wildcard_enum_match_arm
    clippy::unneeded_field_pattern,

    clippy::perf,
//    clippy::undocumented_unsafe_blocks,
    // clippy::unwrap_used,

)]
#![allow(clippy::multiple_crate_versions)] // nothing to be done
#![allow(clippy::option_if_let_else)] // disagree
#![allow(clippy::cast_possible_truncation)] // REMOVE
#![allow(clippy::missing_panics_doc)] // REMOVE
#![allow(clippy::cast_sign_loss)] // REMOVE
#![allow(clippy::redundant_pub_crate)] // broken, conflicts with unreachable_pub
#![allow(clippy::wildcard_imports)] // REMOVE
#![allow(clippy::missing_errors_doc)] // REMOVE
#![allow(clippy::too_many_lines)] // disagree
#![allow(unused_crate_dependencies)] // broken
#![allow(clippy::items_after_statements)] // disagree
#![allow(clippy::match_same_arms)] // disagree
#![allow(clippy::float_cmp)] // I promise to be careful
#![allow(clippy::same_name_method)] // I'm not sure if this is really an issue.
// #[expect(lint_name, reason = "Your explanation here")]
// #[warn(clippy::allow_attributes_without_reason)] // REPLACE

pub mod agg;
pub mod anagram;
pub mod binsearch;
pub mod column;
pub mod comp;
pub mod dyadic;
pub mod expr;
pub mod format;
pub mod hash128;
pub mod input;
pub mod input_file;
pub mod join;
pub mod matcher;
pub mod memory_tracker;
pub mod num;
pub mod output;
pub mod prelude;
pub mod read_line;
pub mod recyclable_vec;
pub mod roman;
pub mod rusty_line;
pub mod sampler;
mod shunting_yard;
pub mod solve;
pub mod sort;
pub mod tabs;
pub mod text;
pub mod textgen;
//mod tokenizer;
mod tok2;
pub mod tooltest;
pub mod trans;
pub mod transpose;
#[cfg(feature = "track")]
pub mod use_memory_tracker;
pub mod util;

pub use crate::input_file::TextFile;
pub use crate::output::LineWriter;
pub use crate::recyclable_vec::RVec;

/// A list of column names.
pub type ColumnNames = Vec<String>;
/// A list of column names, as a parameter reference.
pub type ColumnNamesRef = [String];
/// A list of bytes.
pub type Bytes = Vec<u8>;
/// A list of bytes, as a parameter reference.
pub type BytesRef = [u8];
/// A list of column values.
pub type ColumnValues = RVec<Bytes>;
/// A list of column values, as a parameter reference.
pub type ColumnValuesRef = [Bytes];

/// Get a string from a `ColumnNamesRef`, returning "" if the index is out of bounds.
#[must_use]
pub fn get_str(data: &ColumnNamesRef, index: usize) -> &str {
    data.get(index).map_or("", |s| s.as_str())
}

/// Make a Vec<String> from a list of string literals.
#[macro_export]
macro_rules! svec {
    ($($x:expr),* $(,)?) => {
        vec![$($x.to_string()),*]
    };
}

/// Make useful versions of traits
#[macro_export]
macro_rules! useful {
    ($trait_name:ident) => {
        pastey::paste! {
            /// Useful version of trait $`trait_name`
            pub trait [<Useful $trait_name>]: $trait_name + std::fmt::Debug {}
            impl<T> [<Useful $trait_name>] for T where T: $trait_name + fmt::Debug {}

            /// Safe version of trait $`trait_name`
            pub trait [<Safe $trait_name>]: $trait_name + Send + Sync + std::fmt::Debug {}
            impl<T> [<Safe $trait_name>] for T where T: $trait_name + Send + Sync + std::fmt::Debug {}

            #[doc = " Reference to useful `" $trait_name "` trait."]
            pub type [<$trait_name Ref>] = std::rc::Rc<std::cell::RefCell<dyn [<Useful $trait_name>]>>;
        }
    };
}
