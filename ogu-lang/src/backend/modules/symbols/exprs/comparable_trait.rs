use crate::backend::scopes::symbol::Symbol;
use crate::backend::scopes::types::Type;
use crate::backend::scopes::Scope;
use anyhow::Result;
use crate::backend::modules::types::trait_type::TRAIT_UNKNOWN;

pub(crate) fn resolve_comparable(
    l: &mut Box<dyn Symbol>,
    r: &mut Box<dyn Symbol>,
    scope: &mut dyn Scope,
    tr: &dyn Type,
) -> Result<()> {
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
                println!("A l = {:?} r = {:?}", l, r);
                println!("A lt = {:?} rt = None", lt);
                println!("scope {}", scope.scope_name());
                if lt.promotes(tr) && lt.is_trait() {
                    l.set_type(Some(tr.clone_box()));
                    scope.define(l.clone());
                    r.set_type(Some(tr.clone_box()));
                    scope.define(r.clone());
                    println!("lt promotes {:?}", tr);
                } else {
                    r.set_type(Some(lt.clone()));
                    scope.define(r.clone());
                }

            }
            Some(rt) if lt.is_trait() && &*lt != tr && !rt.is_trait() => {
                println!("B lt = {:?} rt = {:?}", lt, rt);
                l.set_type(Some(rt.clone()));
                scope.define(l.clone());
            }
            Some(rt) if rt.is_trait() && &*rt != tr && !lt.is_trait() => {
                println!("C lt = {:?} rt = {:?}", lt, rt);
                r.set_type(Some(lt.clone()));
                scope.define(r.clone());
            }
            Some(rt) => {
                println!("D TR = {:?}", tr);
                println!("D lt = {:?} rt = {:?}", lt, rt);
                if !lt.is_trait() && rt.is_trait() {
                    r.set_type(Some(lt.clone()));
                    scope.define(r.clone());
                } else if lt.is_trait() && !rt.is_trait() {
                    l.set_type(Some(lt.clone()));
                    scope.define(l.clone());
                } else {
                    if &*lt == TRAIT_UNKNOWN && &*rt == TRAIT_UNKNOWN {
                        l.set_type(Some(tr.clone_box()));
                        scope.define(l.clone());
                        r.set_type(Some(tr.clone_box()));
                        scope.define(r.clone());
                    }
                    else if &*lt == TRAIT_UNKNOWN && &*rt != TRAIT_UNKNOWN {
                        l.set_type(Some(rt.clone()));
                        scope.define(l.clone());
                    } else if &*rt == TRAIT_UNKNOWN && &*lt != TRAIT_UNKNOWN {
                        r.set_type(Some(lt.clone()));
                        scope.define(r.clone());
                    }
                }
                println!("D =>\n l = {:?} => {:?}\n r = {:?} => {:?}", l, l.get_type(), r, r.get_type());
            }
        },
    };
    Ok(())
}
