use crate::backend::scopes::scopes::Scope;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::modules::types::func_type::FuncType;
use crate::parser::ast::expressions::args::Args;
use crate::parser::ast::expressions::expression::Expression;

#[derive(Clone, Debug)]
pub(crate) struct FunctionSym {
    name: String,
    args: Vec<Box<dyn Symbol>>,
    expr: Box<dyn Symbol>,
    ty: Option<Box<dyn Type>>,
}

impl FunctionSym {
    pub(crate) fn new(name: &str, args: &Args, expr: &Expression) -> Box<Self> {
        let ty: Option<Box<dyn Type>> = FuncType::new(args, expr);
        let expr: Box<dyn Symbol> = expr.into();
        let args: Vec<Box<dyn Symbol>> = args.clone().into();
        Box::new(FunctionSym {
            name: name.to_string(),
            args: args.to_vec(),
            expr: expr.clone(),
            ty: ty.clone(),
        })
    }
}

impl Symbol for FunctionSym {
    fn get_name(&self) -> &str {
        &self.name
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.expr.get_type()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.ty = ty.clone()
    }

    fn resolve_type(&mut self, scope: &dyn Scope) -> Option<Box<dyn Type>> {
        match &self.ty {
            Some(ty) => Some(ty.clone()),
            None => {
                self.expr.resolve_type(scope)?;
                self.ty = self.expr.get_type();
                self.get_type()
            }
        }
    }
}

impl<'a> From<Args<'a>> for Vec<Box<dyn Symbol>> {
    fn from(_: Args<'a>) -> Self {
        unimplemented!()
    }
}
