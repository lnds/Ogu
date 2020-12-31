use crate::backend::scopes::scopes::Scope;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::modules::types::func_type::FuncType;
use crate::parser::ast::expressions::args::Args;
use crate::parser::ast::expressions::expression::Expression;
use crate::backend::modules::types::basic_type::BasicType;

#[derive(Clone, Debug)]
pub(crate) struct FunctionSym {
    name: String,
    args: Box<dyn Symbol>,
    expr: Box<dyn Symbol>,
    ty: Option<Box<dyn Type>>,
}

impl FunctionSym {
    pub(crate) fn new(name: &str, args: &Args, expr: &Expression) -> Box<Self> {
        let ty: Option<Box<dyn Type>> = FuncType::new(args, expr);
        let expr: Box<dyn Symbol> = expr.into();
        let args: Box<dyn Symbol> = args.clone().into();
        Box::new(FunctionSym {
            name: name.to_string(),
            args,
            expr,
            ty
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

#[derive(Clone, Debug)]
pub(crate) enum ArgSym {
    Unit,
    Many(Vec<Box<dyn Symbol>>),
}

impl ArgSym {

    fn new_unit() -> Box<ArgSym> {
        Box::new(ArgSym::Unit)
    }
}

impl Symbol for ArgSym {
    fn get_name(&self) -> &str {
        "arg"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        match self {
            ArgSym::Unit => Some(BasicType::unit()),
            _ => todo!()
        }
    }

    fn set_type(&mut self, _ty: Option<Box<dyn Type>>) {
        unimplemented!()
    }

    fn resolve_type(&mut self, _scope: &dyn Scope) -> Option<Box<dyn Type>> {
        todo!()
    }
}


impl<'a> From<Args<'a>> for Box<dyn Symbol> {
    fn from(args: Args<'a>) -> Self {
        match args {
            Args::Void => ArgSym::new_unit(),
            _=> todo!()
        }
    }
}
