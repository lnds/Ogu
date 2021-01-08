use crate::backend::errors::OguError;
use crate::parser::ast::expressions::args::{Arg, Args};
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::expressions::guards::Guard;
use crate::parser::ast::module::body::BodyAst;
use crate::parser::ast::module::decls::{DeclarationAst, FuncType};
use anyhow::{Error, Result};

type GuardType<'a> = (Option<Expression<'a>>, Expression<'a>);

impl<'a> BodyAst<'a> {
    pub(crate) fn merge_vec_of_functions(
        name: &'a str,
        vec_of_funcs: &[DeclarationAst<'a>],
    ) -> Result<DeclarationAst<'a>> {
        if let Some(decl) = Self::match_one_function(vec_of_funcs)? {
            return Ok(decl);
        }
        let (args_names, conds, _ft) = Self::extract_args(name, vec_of_funcs)?;
        let args = args_names
            .clone()
            .into_iter()
            .map(Expression::NameStr)
            .collect();
        let new_args = Args::Many(args_names.into_iter().map(Arg::SimpleStr).collect());
        let new_expr = Expression::CaseExpr(Box::new(Expression::TupleExpr(args)), conds.clone());
        Ok(DeclarationAst::Function(name, new_args, new_expr))
    }

    fn match_one_function(
        vec_of_funcs: &[DeclarationAst<'a>],
    ) -> Result<Option<DeclarationAst<'a>>> {
        if vec_of_funcs.len() != 1 {
            return Ok(None);
        }
        match &vec_of_funcs[0] {
            DeclarationAst::FunctionWithGuards(_, Args::Void, _, _)
            | DeclarationAst::Function(_, Args::Void, _) => Ok(Some(vec_of_funcs[0].clone())),
            DeclarationAst::Function(_, Args::Many(args), _) => {
                if args.iter().all(|a| matches!(a, Arg::Simple(_))) {
                    Ok(Some(vec_of_funcs[0].clone()))
                } else {
                    Ok(None)
                }
            }
            DeclarationAst::FunctionWithGuards(name, Args::Many(args_vec), guards, opt_where) => {
                if !args_vec.iter().all(|a| matches!(a, Arg::Simple(_))) {
                    Ok(None)
                } else {
                    let expr = match opt_where {
                        Some(where_decl) => Expression::LetExpr(
                            where_decl.clone(),
                            Box::new(Guard::guards_to_cond(guards)?),
                        ),
                        None => Guard::guards_to_cond(guards)?,
                    };
                    Ok(Some(DeclarationAst::Function(
                        name,
                        Args::Many(args_vec.clone()),
                        expr,
                    )))
                }
            }
            DeclarationAst::FunctionPrototype(name, _) => Err(Error::new(OguError::SemanticError)
                .context(format!(
                    "Function prototype for {} without function declaration",
                    name
                ))),
            _ => Ok(None),
        }
    }

    fn extract_args(
        name: &'a str,
        vec_of_funcs: &[DeclarationAst<'a>],
    ) -> Result<(
        Vec<String>,
        Vec<GuardType<'a>>,
        Option<FuncType<'a>>,
    )> {
        let mut args_count = 0; // can't be 0 args funcs
        let mut conds = vec![];
        let mut args_names: Vec<String> = vec![];
        let mut function_type = None;
        for fun in vec_of_funcs.iter() {
            let p_args = match fun {
                DeclarationAst::FunctionPrototype(_, ty) => {
                    if function_type.is_some() {
                        return Err(Error::new(OguError::SemanticError).context(format!(
                            "Function declaration for {} already has a function prototype",
                            name
                        )));
                    }
                    function_type = Some(ty.clone());
                    None
                }
                _ => Self::clasify_args(fun, &mut conds),
            };
            if p_args.is_none() {
                continue;
            }
            let p_args = p_args.unwrap();
            let n = if p_args.is_empty() { 1 } else { p_args.len() };
            if args_count == 0 {
                args_count = n;
            }
            if n != args_count {
                return Err(Error::new(OguError::SemanticError)
                    .context("argument count doesn't match with previous function declaration"));
            }
            if args_names.is_empty() {
                args_names = (0..args_count)
                    .into_iter()
                    .map(|n| format!("arg_{}", n))
                    .collect();
            }
            for (p, a) in p_args.iter().enumerate() {
                match a {
                    Arg::Simple(s) => args_names[p] = s.to_string(),
                    Arg::SimpleStr(s) => args_names[p] = s.to_string(),
                    _ => {}
                }
            }
        }
        Ok((args_names, conds, function_type))
    }

    fn clasify_args(
        fun: &DeclarationAst<'a>,
        conds: &mut Vec<(Option<Expression<'a>>, Expression<'a>)>,
    ) -> Option<Vec<Arg<'a>>> {
        match fun {
            DeclarationAst::Function(_, Args::Void, expr) => {
                conds.push((Some(Expression::Unit), expr.clone()));
                Some(vec![])
            }
            DeclarationAst::Function(_, Args::Many(args), expr) => {
                let exprs = Expression::TupleExpr(args.iter().map(BodyAst::arg_to_expr).collect());
                conds.push((Some(exprs), expr.clone()));
                Some(args.to_vec())
            }
            DeclarationAst::FunctionWithGuards(_, args, guards, opt_where) => {
                let (cond_expr, p_args) = match args {
                    Args::Void => (Expression::Unit, vec![]),
                    Args::Many(args) => (
                        Expression::TupleExpr(args.iter().map(BodyAst::arg_to_expr).collect()),
                        args.to_vec(),
                    ),
                };
                for Guard(opt_cond, expr) in guards.iter() {
                    if let Some(cond) = opt_cond {
                        let a_expr = Box::new(if let Some(eq) = opt_where.clone() {
                            Expression::LetExpr(eq, cond.clone())
                        } else {
                            *cond.clone()
                        });
                        let cond_expr =
                            Expression::GuardedExpr(Box::from(cond_expr.clone()), a_expr);
                        conds.push((Some(cond_expr), *expr.clone()));
                    }
                }
                Some(p_args)
            }
            _ => None,
        }
    }

    pub(crate) fn arg_to_expr(arg: &Arg<'a>) -> Expression<'a> {
        match arg {
            Arg::Simple(id) => Expression::Name(id),
            Arg::SimpleStr(id) => Expression::NameStr(id.clone()),
            Arg::Tuple(args) => {
                Expression::TupleExpr(args.iter().map(|a| BodyAst::arg_to_expr(a)).collect())
            }
            Arg::Expr(e) => *e.clone(),
        }
    }
}
