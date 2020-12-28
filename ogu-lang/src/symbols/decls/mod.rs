pub(crate) mod funcs;
pub(crate) mod types;
pub(crate) mod values;

use crate::parser::ast::module::decls::Declaration;
use crate::parser::ast::module::decls::Declaration::{Function, Value};
use crate::symbols::decls::funcs::FunctionSym;
use crate::symbols::decls::values::ValueSym;
use crate::symbols::Symbol;

impl<'a> From<Declaration<'a>> for Box<dyn Symbol> {
    fn from(decl: Declaration<'a>) -> Self {
        match decl {
            Function(name, args, expr) => FunctionSym::new(name, args, expr),
            Value(name, expr) => ValueSym::new(name, expr),
            _d => {
                println!("not implemented for {:?}", _d);
                todo!()
            }
        }
    }
}
