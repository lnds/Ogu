use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::modules::types::basic_type::BasicType;
use crate::parser::ast::expressions::args::Args;
use crate::parser::ast::expressions::expression::Expression;

/*
   FT ::= ()
        | (a) -> FT
        | FT -> T
        |

*/
#[derive(Clone, Debug)]
pub(crate) enum FuncType {
    Param(String),
    Type(Box<dyn Type>),
    Pair(Box<FuncType>, Box<FuncType>),
}

impl FuncType {
    pub(crate) fn new_opt(args: &Args, expr: &Expression) -> Option<Box<dyn Type>> {
        let sym: Box<dyn Symbol> = expr.into();
        let expr_type = sym.get_type()?;
        match args {
            Args::Void => Some(Box::new(FuncType::Pair(
                Box::new(FuncType::Type(BasicType::unit())),
                Box::new(FuncType::Type(expr_type.clone())),
            ))),
            _ => todo!(),
        }
    }
}

impl Type for FuncType {
    fn get_name(&self) -> String {
        match self {
            FuncType::Param(s) => format!("({})", s),
            FuncType::Type(t) => t.get_name(),
            FuncType::Pair(l, r) => format!("{} -> {}", l.get_name(), r.get_name()),
        }
    }

    fn get_signature(&self) -> String {
        format!("FuncType<{}>", self.get_name())
    }
}
