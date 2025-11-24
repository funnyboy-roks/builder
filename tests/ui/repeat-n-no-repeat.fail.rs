use builder::Builder;

#[derive(Builder)]
pub struct Foo {
    #[builder(repeat_n)]
    field_a: String,
}

fn main() {}
