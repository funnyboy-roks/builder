# Builder

A simple builder derive macro, with the intention of accomplishing all
needs from a builder.

```rust
#[derive(Builder)]
pub struct Foo {
    #[builder(default = "42")]
    field_a: u32,
    field_b: bool,
    #[builder(into)]
    field_c: String,
    #[builder(repeat, repeat_n = 1..=3)]
    field_d: Vec<f32>,
}

fn main() {
    let x: Foo = Foo::builder()
        .field_a(69)
        .field_b(true)
        .field_c("hello world")
        .field_d(3.14)
        .field_d(6.28)
        .field_d(2.72)
        .build()
        .unwrap();
}
```
