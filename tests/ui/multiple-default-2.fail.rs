use builder::Builder;

#[derive(Builder)]
pub struct Foo {
    #[builder(default = "String::new()", default)]
    field_a: String,
}

fn main() {}
