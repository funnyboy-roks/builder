use builder::Builder;

#[derive(Debug, Builder)]
pub struct Foo {
    pub field_a: u32,
    pub field_b: bool,
    pub field_c: String,
}

fn main() {
    let x: Foo = Foo::builder()
        .field_a(69)
        .field_b(true)
        .field_c(String::from("hello world"))
        .build()
        .unwrap();

    dbg!(x);
}
