use bauer::Builder;

#[derive(Builder)]
pub struct Foo {
    #[builder(default, default)]
    field_a: String,
}

fn main() {}
