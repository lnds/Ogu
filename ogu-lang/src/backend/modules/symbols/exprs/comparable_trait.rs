use crate::backend::scopes::symbol::Symbol;
use crate::backend::modules::types::trait_type::TraitType;
use anyhow::Result;
use crate::backend::scopes::Scope;
use crate::backend::scopes::types::TypeClone;

pub(crate) fn resolve_comparable(l: &mut Box<dyn Symbol>, r: &mut Box<dyn Symbol>, scope: &mut dyn Scope, tr: &TraitType) -> Result<()> {
    match l.resolve_type(scope)? {
        None => match r.resolve_type(scope)? {
            None => {
                r.set_type(Some(tr.clone_box()));
                l.set_type(Some(tr.clone_box()));
                scope.define(l.clone());
                scope.define(r.clone());
            }
            Some(rt) => {
                l.set_type(Some(rt.clone()));
                scope.define(l.clone());
            }
        },
        Some(lt) => match r.resolve_type(scope)? {
            None => {
                r.set_type(Some(lt.clone()));
                scope.define(r.clone());
            }
            Some(rt) if lt.is_trait() && !rt.is_trait() => {
                l.set_type(Some(rt.clone()));
                scope.define(l.clone());
            }
            Some(rt) if rt.is_trait() && !lt.is_trait() => {
                r.set_type(Some(lt.clone()));
                scope.define(r.clone());
            }
            _ => {}
        },
    };
    Ok(())
}
