# Builder

A simple builder derive macro, with the intention of accomplishing all
needs from a builder.

```rust
#[derive(Builder)]
pub struct Foo {
    field_a: u32,
    field_b: bool,
    field_c: String,
}

fn main() {
    let x: Foo = Foo::builder()
        .field_a(69)
        .field_b(true)
        .field_c(String::from("hello world"))
        .build()
        .unwrap();
}
```
