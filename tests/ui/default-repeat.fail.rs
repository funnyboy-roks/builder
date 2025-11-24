use builder::Builder;

#[derive(Builder)]
pub struct Foo {
    #[builder(default = "String::new()", repeat)]
    field_a: Vec<String>,
}

fn main() {}
