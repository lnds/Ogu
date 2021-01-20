use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{bail, Result};
use crate::backend::modules::symbols::idents::IdSym;
use crate::parser::ast::expressions::expression::ListComprehensionGuard;
use crate::backend::modules::types::list_type::ListType;
use crate::backend::modules::types::tuple_type::TupleType;

#[derive(Debug, Clone)]
pub(crate) enum ListGuard {
    Generator(Box<dyn Symbol>, Box<dyn Symbol>), // id <- expr
    TupleGenerator(Vec<Box<dyn Symbol>>, Box<dyn Symbol>), // (id, id..) <- expr
    Let(Box<dyn Symbol>, Box<dyn Symbol>), // let id = expr
    LetTuple(Vec<Box<dyn Symbol>>, Box<dyn Symbol>), // let id = expr
    Expr(Box<dyn Symbol>), // , expr
}

impl ListGuard {

    pub(crate) fn new_generator(id: Box<dyn Symbol>, list: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ListGuard::Generator(id, list))
    }

    pub(crate) fn new_tuple_generator(ids: Vec<Box<dyn Symbol>>, list: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ListGuard::TupleGenerator(ids, list))
    }

    pub(crate) fn new_let(id: Box<dyn Symbol>, list: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ListGuard::Let(id, list))
    }

    pub(crate) fn new_let_tuple(ids: Vec<Box<dyn Symbol>>, list: Box<dyn Symbol>) -> Box<Self> {
        Box::new(ListGuard::LetTuple(ids, list))
    }

    pub(crate) fn new_expr(expr:Box<dyn Symbol>) -> Box<Self> {
        Box::new(ListGuard::Expr(expr))
    }
}

impl Symbol for ListGuard {
    fn get_name(&self) -> &str {
        "list_guard"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
       match &self {
           ListGuard::Let(id, l)|
           ListGuard::Generator(id, l) => {
               match id.get_type() {
                   None => l.get_type(),
                   Some(_) => id.get_type()
               }
           }
           ListGuard::LetTuple(t, _) |
           ListGuard::TupleGenerator(t, _) => {
               if t.iter().any(|e| e.get_type().is_none()) {
                   None
               } else {
                   Some(TupleType::new_box(t.iter().flat_map(|e|e.get_type()).collect()))
               }
           }
           ListGuard::Expr(e) => e.get_type()
       }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match self {
            ListGuard::Generator(id, lst) => {
                lst.resolve_type(scope)?;
                if let Some(lt) = lst.get_type() {
                    if let Some(lt) = lt.downcast_ref::<ListType>() {
                        match lt {
                            ListType::EmptyList => bail!("can't generate anything from an empty list!"),
                            ListType::List(tty) => {
                                match id.get_type() {
                                    None => id.set_type(Some(tty.clone_box())),
                                    Some(ty) => {
                                        if ty != tty.clone() {
                                            id.matches_types(Some(tty.clone_box()));
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        bail!("generator must come from a list");
                    }
                }
                scope.define(id.clone());
            }
            ListGuard::Expr(expr) => {
                expr.resolve_type(scope)?;
            }
            _ => todo!()
        }
        Ok(self.get_type())
    }

    fn define_into(&self, scope: &mut dyn Scope) {
        match self {
            ListGuard::Generator(id, _) => {
                id.define_into(scope);
            }
            ListGuard::Expr(expr) => {
                expr.define_into(scope);
            }
            _ => todo!()
        }
    }

    fn is_seq(&self) -> bool {
        true
    }

}



impl<'a> From<&ListComprehensionGuard<'a>> for Box<dyn Symbol> {
    fn from(guard: &ListComprehensionGuard<'a>) -> Self {
        match guard {
            ListComprehensionGuard::Generator(id, e) =>
                ListGuard::new_generator(IdSym::new(id), e.into()),
            ListComprehensionGuard::TupleGenerator(ids, e) => {
                let ids = ids.iter().map(|id|
                    IdSym::new_id(id)
                ).collect();
                ListGuard::new_tuple_generator(ids, e.into())
            }
            ListComprehensionGuard::Let(id, e) =>
                ListGuard::new_let(IdSym::new(id), e.into()),
            ListComprehensionGuard::LetTuple(ids, e) => {
                let ids = ids.iter().map(|id|
                    IdSym::new_id(id)
                ).collect();
                ListGuard::new_let_tuple(ids, e.into())
            },
            ListComprehensionGuard::Expr(e) =>
                ListGuard::new_expr(e.into())
        }
    }



}