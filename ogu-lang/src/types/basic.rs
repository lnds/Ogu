use crate::types::Type;

#[derive(Clone)]
pub(crate) enum BasicType {
    Unit,
    Primitive(String),
    Str,
}

impl BasicType {

    pub fn unit() -> Box<BasicType> {
        Box::new(BasicType::Unit)
    }

    pub fn primitive(ty: &str) -> Box<BasicType> {
        Box::new(BasicType::Primitive(ty.to_string()))
    }

    pub fn int(s: &str) -> Box<BasicType> {
        if s.ends_with("i8") {
            BasicType::primitive("i8")
        } else if s.ends_with("i16") {
            BasicType::primitive("i16")
        } else if s.ends_with("i32") {
            BasicType::primitive("i32")
        } else if s.ends_with("i64") {
            BasicType::primitive("i64")
        } else if s.ends_with("i128") {
            BasicType::primitive("i128")
        } else if s.ends_with("isize") {
            BasicType::primitive("isize")
        } else if s.ends_with("u8") {
            BasicType::primitive("u8")
        } else if s.ends_with("u16") {
            BasicType::primitive("u16")
        } else if s.ends_with("u32") {
            BasicType::primitive("u32")
        } else if s.ends_with("u64") {
            BasicType::primitive("u64")
        } else if s.ends_with("u128") {
            BasicType::primitive("u128")
        } else if s.ends_with("usize") {
            BasicType::primitive("usize")
        } else if s.ends_with('N') {
            println!("Big integer not implemented");
            todo!()
        } else {
            BasicType::primitive("i64")
        }
    }

}

impl Type for BasicType {
    fn get_name(&self) -> String {
        match self {
            BasicType::Unit => String::from("()"),
            BasicType::Primitive(s) => s.clone(),
            BasicType::Str => String::from("String"),
        }
    }
}
