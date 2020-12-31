use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::modules::types::basic_type::BasicType;
use crate::parser::ast::expressions::args::Args;
use crate::parser::ast::expressions::expression::Expression;
use crate::backend::modules::symbols::funcs::ArgsSym;

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
    Pair(Box<dyn Type>, Box<dyn Type>),
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

    pub(crate) fn make(args: &ArgsSym, expr: &dyn Symbol) -> Option<Box<dyn Type>> {
        match args {
            ArgsSym::Unit => Self::make_pair(Self::make_box_type(BasicType::unit()), Self::get_type(expr)?),
            ArgsSym::Many(args) if args.len() == 1 =>
                Self::make_pair(Self::get_type(&*args[0])?, FuncType::get_type(expr)?),
            ArgsSym::Many(args) if args.len() == 2 =>
                Self::make_pair(
                    Self::make_pair(Self::get_type(&*args[0])?, Self::get_type(&*args[1])?)?,
                     FuncType::get_type(expr)?),

            _ => todo!()
        }
    }

    fn get_type(sym: &dyn Symbol) -> Option<Box<FuncType>> {
        let ty = sym.get_type()?;
        Self::make_type(ty)
    }

    pub(crate) fn make_box_type(ty: Box<dyn Type>) -> Box<FuncType> {
        Box::new(FuncType::Type(ty))
    }

    pub(crate) fn make_type(ty: Box<dyn Type>) -> Option<Box<FuncType>> {
        Some(Self::make_box_type(ty))
    }

    pub(crate) fn make_pair_box(t1: Box<dyn Type>, t2: Box::<dyn Type>) -> Box<dyn Type> {
        Box::new(FuncType::Pair(t1, t2))
    }

    pub(crate) fn make_pair(t1: Box<dyn Type>, t2: Box::<dyn Type>) -> Option<Box<dyn Type>> {
        Some(Self::make_pair_box(t1, t2))
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
