use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::{Type, TypeComparation};
use anyhow::{bail, Result};
use crate::backend::modules::symbols::idents::IdSym;

pub(crate) mod func_call;
pub(crate) mod func_compose;
pub(crate) mod func_def;
pub(crate) mod lambda_expr;
pub(crate) mod recur_call;

type OptType = Option<Box<dyn Type>>;
type SymbolVec = [Box<dyn Symbol>];

pub(crate) fn is_subtype(expr_name: &str, a: OptType, b: OptType) -> Result<bool> {
    match a {
        None => match b {
            None => Ok(true),
            Some(_) => Ok(true),
        },
        Some(at) => match b {
            None => Ok(true),
            Some(bt) => {
                let c = at.compare(&*bt);
                match c {
                    TypeComparation::Superior => Ok(false),
                    TypeComparation::Incomparables => bail!(
                        "incompatible type for arg substitution in {}, expecting {}, found {}",
                        expr_name,
                        at.get_name(),
                        bt.get_name()
                    ),
                    _ => Ok(true),
                }
            }
        },
    }
}

fn check_can_replace_args(expr_name: &str, own_args: &SymbolVec, args: &SymbolVec) -> Result<()> {
    if own_args.len() != args.len() {
        bail!("wrong arguments passed")
    }
    for (a, b) in own_args.iter().zip(args.iter()) {
        if let (Some(ta), Some(tb)) = (a.get_type(), b.get_type()) {
            if !ta.is_compatible_with(&*tb) && !tb.is_compatible_with(&*ta) {
                bail!(
                    "incompatible args passed for {}\n TA = {:?}\n TB = {:?}",
                    expr_name,
                    ta,
                    tb
                )
            }
        }
    }
    Ok(())
}

fn swap_args(msg: &str, own_args: &SymbolVec, args: &SymbolVec) -> Result<Vec<Box<dyn Symbol>>> {
    check_can_replace_args(msg, own_args, args)?;
    let mut new_args: Vec<Box<dyn Symbol>> = vec![];
    for (a, b) in own_args.iter().zip(args.iter()) {
        if is_subtype(msg, a.get_type(), b.get_type())? {
            new_args.push(IdSym::new_with_type(a.get_name(), b.get_type().clone()))
        } else {
            new_args.push(a.clone())
        }
    }
    Ok(new_args)
}