use crate::backend::errors::OguError;
use crate::backend::modules::types::tuple_type::TupleType;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone};
use crate::backend::scopes::Scope;
use anyhow::{Error, Result};

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
        /*
        match &self.ty {
            None => self.ty = ty.clone(),
            Some(t) if t.is_trait() => self.ty = ty.clone(),
            _ => {}
        }*/
    }

    fn matches_types(&mut self, arg_ty: Option<Box<dyn Type>>) {
        println!("MATCHES TYPES {:?}", self.name);
        if let Some(ty) = &arg_ty {
            if let Some(tt) = ty.downcast_ref::<TupleType>() {
                if let Some(sty) = &self.ty {
                    if let Some(stt) = sty.downcast_ref::<TupleType>() {
                        println!("STT = {:?}", stt);
                        println!("TT = {:?}", tt);
                        let mut stt = stt.clone();
                        stt.match_types(tt);
                        self.ty = Some(stt.clone_box());
                    } else {
                        self.ty = arg_ty.clone();
                    }
                }
            } else {
                self.ty = arg_ty.clone();
            }
        } else {
            self.ty = arg_ty.clone();
        }
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        match &self.ty {
            Some(ty) if !ty.is_trait() => Ok(Some(ty.clone())),
            _ => match scope.resolve(&self.name) {
                None => Err(Error::new(OguError::SymbolTableError)
                    .context(format!("Symbol not found : {}", self.name))),
                Some(sym) => {
                    self.ty = sym.get_type();
                    Ok(self.get_type())
                }
            },
        }
    }

    fn storable(&self) -> bool {
        true
    }
}
