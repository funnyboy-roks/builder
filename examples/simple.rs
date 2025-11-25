use builder::Builder;

#[derive(Debug, Builder)]
#[builder(kind = "owned", prefix = "set_")]
pub struct Foo {
    #[builder(default = "42")]
    pub field_a: u32,
    pub field_b: bool,
    #[builder(into)]
    pub field_c: String,
    #[builder(skip_prefix, skip_suffix, rename = "add_d", repeat, repeat_n = 1..=3)]
    pub field_d: Vec<f64>,
}

fn main() {
    let x: Foo = Foo::builder()
        .set_field_a(69)
        .set_field_b(true)
        .set_field_c("hello world")
        .add_d(std::f64::consts::PI)
        .add_d(std::f64::consts::TAU)
        .add_d(2.72)
        .build()
        .unwrap();
    dbg!(x);
}
