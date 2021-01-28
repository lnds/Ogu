#![allow(clippy::transmute_ptr_to_ref)]

use std::fmt::Debug;

use anyhow::Result;

use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;

pub(crate) trait Symbol: SymbolClone + Debug + mopa::Any {
    fn get_name(&self) -> &str;
    fn get_type(&self) -> Option<Box<dyn Type>>;
    fn set_type(&mut self, _: Option<Box<dyn Type>>) {}
    fn matches_types(&mut self, ty: Option<Box<dyn Type>>) {
        self.set_type(ty)
    }
    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>>;
    fn storable(&self) -> bool {
        false
    }
    fn set_storable(&mut self, _: bool) {}
    fn get_curry(&self) -> Option<Box<dyn Symbol>> {
        None
    }
    fn define_into(&self, scope: &mut dyn Scope) -> Option<Box<dyn Symbol>> {
        scope.define(self.clone_box())
    }

    fn is_seq(&self) -> bool {
        false
    } // for tuples, list, arrays, etc
    fn get_seq(&self) -> Option<Vec<Box<dyn Symbol>>> {
        None
    }
    fn set_seq(&mut self, _: Option<Vec<Box<dyn Symbol>>>) {}
}

mopafy!(Symbol);

pub(crate) trait SymbolClone {
    fn clone_box(&self) -> Box<dyn Symbol>;
}

impl<T> SymbolClone for T
where
    T: 'static + Symbol + Clone,
{
    fn clone_box(&self) -> Box<dyn Symbol> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Symbol> {
    fn clone(&self) -> Box<dyn Symbol> {
        self.clone_box()
    }
}
