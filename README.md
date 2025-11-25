<!-- Readme generated with `cargo-readme`: https://github.com/webern/cargo-readme -->

# bauer

[![Crates.io](https://img.shields.io/crates/v/bauer.svg)](https://crates.io/crates/bauer)
[![Documentation](https://docs.rs/bauer/badge.svg)](https://docs.rs/bauer/)
[![Dependency status](https://deps.rs/repo/github/funnyboy-roks/bauer/status.svg)](https://deps.rs/repo/github/funnyboy-roks/bauer)

A derive macro for automatically generating the builder pattern

```rust
use bauer::Builder;

#[derive(Builder)]
pub struct Foo {
    #[builder(default = "42")]
    field_a: u32,
    field_b: bool,
    #[builder(into)]
    field_c: String,
    #[builder(repeat, repeat_n = 1..4)]
    field_d: Vec<f32>,
}

let foo: Foo = Foo::builder()
    .field_b(true)
    .field_c("hello world")
    .field_d(3.14)
    .field_d(6.28)
    .field_d(2.72)
    .build()
    .unwrap();

assert_eq!(
    foo,
    Foo {
        field_a: 42,
        field_b: true,
        field_c: String::from("hello world"),
        field_d: vec![3.14, 6.28, 2.72],
    },
);
```
