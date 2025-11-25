use bauer::Builder;

#[derive(Debug, Builder)]
pub struct Foo<T> {
    pub t: T,
}

fn main() {
    let x: Foo<u32> = Foo::builder().t(69).build().unwrap();
    dbg!(x);
}
