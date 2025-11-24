use builder::Builder;

#[derive(Builder)]
pub struct Foo {
    #[builder(default = "String::new()", default = "String::from(\"hi\")")]
    field_a: String,
}

fn main() {}
