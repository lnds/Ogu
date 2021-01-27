use anyhow::{bail, Result};

use crate::backend::modules::types::list_type::ListType;
use crate::backend::modules::types::tuple_type::TupleType;
use crate::backend::scopes::Scope;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone};

#[derive(Clone, Debug)]
pub(crate) struct IdSym {
    name: String,
    ty: Option<Box<dyn Type>>,
}

impl IdSym {
    pub(crate) fn new(id: &str) -> Box<Self> {
        Box::new(IdSym {
            name: id.to_string(),
            ty: None,
        })
    }

    pub(crate) fn new_id(id: &str) -> Box<dyn Symbol> {
        Box::new(IdSym {
            name: id.to_string(),
            ty: None,
        })
    }

    pub(crate) fn new_with_type(id: &str, ty: Option<Box<dyn Type>>) -> Box<Self> {
        Box::new(IdSym {
            name: id.to_string(),
            ty,
        })
    }
}

impl Symbol for IdSym {
    fn get_name(&self) -> &str {
        &self.name
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        self.ty.clone()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        self.ty = ty.clone();
    }

    fn matches_types(&mut self, arg_ty: Option<Box<dyn Type>>) {
        if let Some(ty) = &arg_ty {
            match ty.downcast_ref::<TupleType>() {
                None => self.ty = arg_ty.clone(),
                Some(tt) => {
                    if let Some(sty) = &self.ty {
                        match sty.downcast_ref::<TupleType>() {
                            None => self.ty = arg_ty.clone(),
                            Some(stt) => {
                                let mut stt = stt.clone();
                                stt.match_types(tt);
                                self.ty = Some(stt.clone_box());
                            }
                        }
                    } else {
                        self.ty = arg_ty.clone();
                    }
                }
            }
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        let t = match &self.ty {
            Some(ty) if !ty.is_trait() => Ok(Some(ty.clone())),
            Some(_) => match scope.resolve(&self.name) {
                None => Ok(self.get_type()),
                Some(sym) => match sym.get_type() {
                    None => Ok(self.get_type()),
                    Some(new_ty) => {
                        self.ty = Some(new_ty);
                        Ok(self.get_type())
                    }
                },
            },
            None => match scope.resolve(&self.name) {
                None => bail!("Symbol not found : {}", self.name),
                Some(sym) => {
                    self.ty = sym.get_type();
                    Ok(self.get_type())
                }
            },
        };
        t
    }

    fn storable(&self) -> bool {
        true
    }
}
