use crate::backend::errors::OguError;
use crate::backend::modules::types::basic_type::INVALID_TYPE;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::{Error, Result};

#[derive(Clone, Debug)]
pub(crate) struct RepeatExpr {
    reps: Vec<Box<dyn Symbol>>,
}

impl RepeatExpr {
    pub(crate) fn new(
        reps: Vec<Box<dyn Symbol>>
    ) -> Box<Self> {
        Box::new(RepeatExpr {
            reps,
        })
    }
}

impl Symbol for RepeatExpr {
    fn get_name(&self) -> &str {
        "repeat_expr"
    }

    fn get_type(&self) -> Option<Box<dyn Type>> {
        todo!()
    }

    fn set_type(&mut self, ty: Option<Box<dyn Type>>) {
        todo!()
    }

    fn resolve_type(&mut self, scope: &mut dyn Scope) -> Result<Option<Box<dyn Type>>> {
        Ok(self.get_type())
    }
}
