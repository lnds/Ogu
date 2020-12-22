use std::collections::{HashMap, HashSet};
use crate::symbols::types::Type;
use crate::parser::ast::module::ModuleAst;

pub(crate) struct Module<'a> {
    name: &'a str,
    exports: ModuleExports<'a>,
    funcs: HashMap<&'a str, Func>,
    types: HashMap<&'a str, Type>,
    macros: HashMap<&'a str, Macro>,
}

impl<'a> Module<'a> {

    pub fn new(_module: &ModuleAst) -> Self {
        todo!()
    }
}

pub(crate) enum ModuleExports<'a> {
    Nothing,
    All,
    Names(HashSet<&'a str>)
}

struct Func {

}

struct Macro {

}
