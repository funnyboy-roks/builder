use bauer::Builder;

#[derive(Debug, Builder, PartialEq)]
#[builder(kind = "borrowed")]
pub struct Foo {
    pub field_a: u32,
    #[builder(default)]
    pub field_b: u32,
    #[builder(default = "42")]
    pub field_c: u32,
    #[builder(default, into)]
    pub field_d: String,
    #[builder(default = "\"hello\"", into)]
    pub field_e: String,
    #[builder(into)]
    pub field_f: String,
    #[builder(repeat)]
    pub field_g: Vec<u32>,
    #[builder(repeat, rename = "field_h_single")]
    pub field_h: Vec<u32>,
    #[builder(repeat, repeat_n = 1..=3)]
    pub field_i: Vec<u32>,
}

fn main() {
    let x = Foo::builder()
        .field_a(5)
        .field_f("world")
        .field_g(0)
        .field_g(1)
        .field_h_single(2)
        .field_h_single(3)
        .field_i(4)
        .field_i(5)
        .field_i(6)
        .build()
        .unwrap();

    dbg!(&x);

    assert_eq!(
        x,
        Foo {
            field_a: 5,
            field_b: 0,
            field_c: 42,
            field_d: String::from(""),
            field_e: String::from("hello"),
            field_f: String::from("world"),
            field_g: vec![0, 1],
            field_h: vec![2, 3],
            field_i: vec![4, 5, 6],
        }
    );
}
