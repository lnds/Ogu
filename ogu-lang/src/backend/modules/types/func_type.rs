use crate::backend::modules::types::basic_type::BasicType;
use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;
use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeClone, TypeComparation};
use crate::parser::ast::module::decls::FuncTypeAst;
use anyhow::{bail, Result};

#[derive(Clone, Debug)]
pub(crate) struct FuncType {
    pub(crate) args: Option<Vec<Box<dyn Type>>>,
    pub(crate) result: Box<dyn Type>,
}

impl Type for FuncType {
    fn get_name(&self) -> String {
        format!("[{:?}] -> {:?}", self.args, self.result)
    }

    fn get_signature(&self) -> String {
        format!("FuncType<{}>", self.get_name())
    }

    fn resolve_expr_type(&self) -> Option<Box<dyn Type>> {
        Some(self.result.clone())
    }

    fn is_compatible_with(&self, other: &dyn Type) -> bool {
        let r = match other.downcast_ref::<FuncType>() {
            None => false,
            Some(other) => self.result.is_compatible_with(&*other.result),
        };
        r
    }

    fn is_trait(&self) -> bool {
        match &self.args {
            None => self.result.is_trait(),
            Some(args) => {
                for a in args.iter() {
                    if a.is_trait() {
                        return true;
                    }
                }
                self.result.is_trait()
            }
        }
    }

    fn compare(&self, other: &dyn Type) -> TypeComparation {
        match other.downcast_ref::<FuncType>() {
            None => TypeComparation::Incomparables,
            Some(other) => self.result.compare(&*other.result),
        }
    }
}

impl FuncType {
    pub(crate) fn new_func_type(
        args: Option<Vec<Box<dyn Type>>>,
        result: Box<dyn Type>,
    ) -> Box<dyn Type> {
        Box::new(FuncType { args, result })
    }


    #[allow(dead_code)]
    pub(crate) fn new_opt(
        args: Option<Vec<Box<dyn Type>>>,
        result: Box<dyn Type>,
    ) -> Option<Box<dyn Type>> {
        Some(Self::new_func_type(args, result))
    }


    pub(crate) fn new_box(
        args: Option<Vec<Box<dyn Type>>>,
        result: Box<dyn Type>,
    ) -> Box<Self> {
        Box::new(FuncType { args, result })
    }



    pub(crate) fn get_args(&self) -> Option<Vec<Box<dyn Type>>> {
        self.args.clone()
    }

    pub(crate) fn make(
        args: &Option<Vec<Box<dyn Symbol>>>,
        expr: &dyn Symbol,
    ) -> Option<Box<Self>> {
        let result = expr.get_type()?;
        let args = match args {
            None => None,
            Some(a) => Some(
                a.iter()
                    .map(|sym| sym.get_type().unwrap_or_else(|| TRAIT_UNKNOWN.clone_box()))
                    .collect(),
            ),
        };
        Some(Box::new(FuncType { args, result }))
    }

    pub(crate) fn check_and_make(
        name: &str,
        ft: &FuncTypeAst,
        args: &mut Option<Vec<Box<dyn Symbol>>>,
    ) -> Result<Option<Box<Self>>> {
        let (v, t) = Self::expand_from_ast_ft(ft, vec![]);
        match args {
            None if !v.is_empty() => bail!(
                "prototype define no args, but args given for func : {}",
                name
            ),
            None => Ok(Some(Box::new(FuncType {
                args: None,
                result: t.clone(),
            }))),
            Some(args) if v.len() != args.len() => bail!(
                "args count doesn't match with prototype for function {}",
                name
            ),
            Some(args) => {
                for (p, a) in args.iter_mut().enumerate() {
                    a.set_type(Some(v[p].clone()));
                }
                Ok(Some(Box::new(FuncType {
                    args: Some(v.to_vec()),
                    result: t.clone(),
                })))
            }
        }
    }

    fn expand_from_ast_ft(
        ft: &FuncTypeAst,
        vec: Vec<Box<dyn Type>>,
    ) -> (Vec<Box<dyn Type>>, Box<dyn Type>) {
        match ft {
            FuncTypeAst::Void => (vec, BasicType::unit()),
            FuncTypeAst::Simple("Int") => (vec, BasicType::int()), // TODO must resolve in symbol table
            FuncTypeAst::Simple("UInt") => (vec, BasicType::uint()), // TODO must resolve in symbol table
            FuncTypeAst::Simple(str) => (vec, BasicType::undefined(str)),
            FuncTypeAst::Chain(t1, t2) => {
                let (mut v1, tt1) = Self::expand_from_ast_ft(&*t1, vec![]);
                let (mut v2, tt2) = Self::expand_from_ast_ft(&*t2, vec![]);
                v1.push(tt1);
                v1.append(&mut v2);
                (v1, tt2)
            }
            _ => todo!("the rest"),
        }
    }
}
