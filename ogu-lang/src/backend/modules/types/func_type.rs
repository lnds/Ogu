use crate::backend::modules::symbols::funcs::{ArgsSym, vec_args_into};
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::parser::ast::expressions::args::Args;
use crate::parser::ast::expressions::expression::Expression;

#[derive(Clone, Debug)]
pub(crate) struct FuncType {
    args: Option<Vec<Box<dyn Type>>>,
    result: Box<dyn Type>,
}

impl Type for FuncType {
    fn get_name(&self) -> String {
        format!("[{:?}] -> {:?}", self.args, self.result)
    }

    fn get_signature(&self) -> String {
        format!("FuncType<{}>", self.get_name())
    }

    fn is_trait(&self) -> bool {
        self.result.is_trait()
    }

    fn resolve_expr_type(&self) -> Option<Box<dyn Type>> {
        Some(self.result.clone())
    }
}


impl FuncType {

    pub(crate) fn new(args: Option<Vec<Box<dyn Type>>>, result: Box<dyn Type>) -> Box<Self> {
        Box::new(FuncType { args, result })
    }

    pub(crate) fn new_opt(args: Option<Vec<Box<dyn Type>>>, result: Box<dyn Type>) -> Option<Box<dyn Type>> {
        Some(Self::new(args, result))
    }


    pub(crate) fn from_ast_opt(args: &Args, expr: &Expression) -> Option<Box<dyn Type>> {
        let sym: Box<dyn Symbol> = expr.into();
        let result = sym.get_type()?;
        let args = match args {
            Args::Void => None,
            Args::Many(a) => Some(vec_args_into(a).iter().flat_map(|sym| sym.get_type()).collect())
        };
        Self::new_opt(args, result)
    }


    pub(crate) fn make(args: &ArgsSym, expr: &dyn Symbol) -> Option<Box<dyn Type>> {
        println!("make func type args = {:?} expr = {:?}", args, expr);
        let result = expr.get_type()?;
        let args = match args {
            ArgsSym::Unit => None,
            ArgsSym::Many(a) => {
                println!("make from many a = {:#?}", a);
                Some(a.iter().flat_map(|sym| sym.get_type()).collect())
            }
        };
        Self::new_opt(args, result)
    }
}
