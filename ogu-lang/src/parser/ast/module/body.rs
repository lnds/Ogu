use crate::lexer::tokens::Lexeme;
use crate::parser::ast::module::decls::{DeclParseResult, DeclVec, DeclarationAst};
use crate::parser::{consume_symbol, raise_parser_error, Parser};
use anyhow::{Error, Result};
use std::collections::HashMap;
use crate::backend::errors::OguError;
use crate::parser::ast::expressions::args::{Args, Arg};
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::expressions::args::Arg::Expr;

#[derive(Debug, Clone)]
pub(crate) struct BodyAst<'a> {
    pub(crate) declarations: Vec<DeclarationAst<'a>>,
}

impl<'a> BodyAst<'a> {
    pub(crate) fn get_decls(self) -> Vec<DeclarationAst<'a>> {
        self.declarations.to_vec()
    }

    pub(crate) fn parse(parser: &'a Parser<'a>, pos: usize) -> Result<BodyAst<'a>> {
        let declarations = BodyAst::parse_decls(parser, pos)?;
        Ok(BodyAst { declarations })
    }

    fn parse_decls(parser: &'a Parser<'a>, pos: usize) -> Result<DeclVec<'a>> {
        let mut result = vec![];
        let mut pos = pos;
        while let Some((decl, new_pos)) = BodyAst::parse_decl(parser, pos)? {
            result.push(decl);
            pos = new_pos;
        }
        BodyAst::merge_functions(result)
    }

    fn merge_functions(decls: DeclVec<'a>) -> Result<DeclVec<'a>> {
        let mut funcs = HashMap::new();
        for decl in decls.iter() {
            if let DeclarationAst::Function(name, _,_) = decl {
                funcs.entry(name).or_insert(vec![]).push(decl.clone());
            }
        }
        let mut result = vec![];
        for decl in decls.iter() {
            if let DeclarationAst::Function(name, _,_) = decl {
                if let Some((_, vec_of_func)) = funcs.remove_entry(&name) {
                    result.push(BodyAst::merge_vec_of_functions(&name, &vec_of_func)?);
                }
            } else {
                result.push(decl.clone());
            }
        }
        println!("MERGE : {:#?}", result);
        Ok(result)
    }

    fn merge_vec_of_functions(name: &'a  str, vec_of_funcs: &DeclVec<'a>) -> Result<DeclarationAst<'a>> {
        if vec_of_funcs.len() == 1 {
            if let DeclarationAst::Function(_, args, _) = &vec_of_funcs[0] {
                match args {
                    Args::Void => return Ok(vec_of_funcs[0].clone()),
                    Args::Many(args) => {
                        if args.iter().all(|a| matches!(a, Arg::Simple(_))) {
                            return Ok(vec_of_funcs[0].clone())
                        }
                    }
                }
            }
        }
        println!("MERGE VEC OF FUNC {:#?}", vec_of_funcs);
        let mut args_count = 0; // can't be 0 args funcs
        let mut conds = vec![];
        for fun in vec_of_funcs.iter() {
            if let DeclarationAst::Function(_, args, expr) = fun {
                let n = match args {
                    Args::Void => 1,
                    Args::Many(args) => args.len()
                };

                if args_count == 0 {
                    args_count = n;
                }
                if n != args_count {
                    return Err(Error::new(OguError::SemanticError).context("argument count doesn't match with previous function declaration"))
                }

                let cond = match args {
                    Args::Void => (Some(Expression::Unit), expr.clone()),
                    Args::Many(args) => {
                        let exprs = Expression::TupleExpr(args.iter().map(BodyAst::arg_to_expr).collect());
                        (Some(exprs), expr.clone())
                    }
                };
                conds.push(cond);
            }
        }

        let args_names : Vec<String> = (0..args_count).into_iter().map(|n| format!("arg_{}", n)).collect();
        let new_args = Args::Many(args_names.into_iter().map(|s| Arg::SimpleStr(s.clone())).collect());
        let args_names : Vec<String> = (0..args_count).into_iter().map(|n| format!("arg_{}", n)).collect();

        let new_expr = Expression::CaseExpr(
            Box::new(Expression::TupleExpr(args_names.into_iter().map(|s| Expression::NameStr(s.clone())).collect())),
            conds.clone());
        Ok(DeclarationAst::Function(name, new_args, new_expr))
    }

    fn arg_to_expr(arg: &Arg<'a>) -> Expression<'a> {
        match arg {
            Arg::Simple(id) => Expression::Name(id),
            Arg::SimpleStr(id) => Expression::NameStr(id.clone()),
            Arg::Tuple(args) => Expression::TupleExpr(args.iter().map(|a| BodyAst::arg_to_expr(a)).collect()),
            Expr(e) => *e.clone()
        }
    }

    pub fn parse_decl(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
        let pos = parser.skip_nl(pos);
        match parser.get_token(pos) {
            None => Ok(None),
            Some(Lexeme::Macro) => {
                let pos = consume_symbol(parser, pos, Lexeme::Macro)?;
                if let Some((decl, pos)) = BodyAst::parse_decl(parser, pos)? {
                    Ok(Some((DeclarationAst::MacroDecl(Box::new(decl)), pos)))
                } else {
                    raise_parser_error("expecting macro declaration", parser, pos, false)
                }
            }
            Some(Lexeme::Id(_)) if parser.peek(pos + 1, Lexeme::Colon) => {
                let (proto, pos) = DeclarationAst::parse_func_prototype(parser, pos)?;
                Ok(Some((DeclarationAst::FunctionPrototype(proto), pos)))
            }
            Some(Lexeme::Id(_)) => DeclarationAst::parse_func_or_val(parser, pos),
            Some(Lexeme::Type) => DeclarationAst::parse_type_decl(parser, pos),
            Some(Lexeme::Alias) => DeclarationAst::parse_type_alias_decl(parser, pos),
            Some(Lexeme::Trait) => DeclarationAst::parse_trait_decl(parser, pos),
            Some(Lexeme::Handler) => DeclarationAst::parse_handler_decl(parser, pos),
            Some(Lexeme::Effect) => {
                let (prot, pos) = DeclarationAst::parse_effect_func_prototype(parser, pos)?;
                Ok(Some((DeclarationAst::Effect(prot), pos)))
            }
            Some(Lexeme::Extends) => DeclarationAst::parse_extends(parser, pos),
            Some(Lexeme::LargeString(i)) => Ok(Some((
                DeclarationAst::DocString(parser.get_large_string(i)),
                pos + 1,
            ))),
            _ => raise_parser_error("expecting a declaration", parser, pos, true),
        }
    }
}
