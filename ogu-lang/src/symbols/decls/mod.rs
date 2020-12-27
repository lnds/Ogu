pub(crate) mod funcs;

use crate::parser::ast::module::body::Declaration;
use crate::parser::ast::module::body::Declaration::Function;
use crate::symbols::decls::funcs::FunctionSym;
use crate::symbols::Symbol;

impl<'a> From<Declaration<'a>> for Box<dyn Symbol> {
    fn from(decl: Declaration<'a>) -> Self {
        match decl {
            Function(name, args, expr) => FunctionSym::new(name, args, expr),
            _d => {
                println!("not implemented for {:?}", _d);
                todo!()
            }
        }
    }
}
