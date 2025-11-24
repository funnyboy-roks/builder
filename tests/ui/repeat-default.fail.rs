use builder::Builder;

#[derive(Builder)]
pub struct Foo {
    #[builder(repeat, default = "String::new()")]
    field_a: Vec<String>,
}

fn main() {}
