use anyhow::Result;

use crate::lexer::tokens::Lexeme;
use crate::parser::ast::expressions::args::Arg;
use crate::parser::ast::expressions::consume_id;
use crate::parser::ast::expressions::equations::Equation;
use crate::parser::ast::expressions::expression::Expression;
use crate::parser::ast::module::decls::{
    AlgebraicElement, AlgebraicType, BaseType, DeclParseResult, DeclarationAst, Derivation,
    FuncType, RecordElement,
};
use crate::parser::{
    consume_string, consume_symbol, consume_type_id, parse_opt_dedent, parse_opt_indent,
    raise_parser_error, Parser,
};

impl<'a> DeclarationAst<'a> {
    pub(crate) fn parse_type_decl(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Type)?;
        if parser.peek(pos, Lexeme::Alias) {
            return DeclarationAst::parse_type_alias_decl(parser, pos);
        }

        let (type_id, pos) = consume_type_id(parser, pos)?;
        let (type_args, pos) =
            if parser.peek(pos, Lexeme::Assign) || parser.peek(pos, Lexeme::NewLine) {
                (None, pos)
            } else {
                let (args, pos) = DeclarationAst::parse_type_args(parser, pos)?;
                (Some(args), pos)
            };
        let (top_indent, pos) = parse_opt_indent(parser, pos);
        let pos = consume_symbol(parser, pos, Lexeme::Assign)?;
        let (type_decl, pos) = DeclarationAst::parse_algebraic_type(parser, pos)?;
        let mut algebraic_elements = vec![type_decl];
        let (in_indent, mut pos) = parse_opt_indent(parser, pos);
        while parser.peek(pos, Lexeme::Guard) {
            let new_pos = consume_symbol(parser, pos, Lexeme::Guard)?;
            let (type_decl, new_pos) = DeclarationAst::parse_algebraic_type(parser, new_pos)?;
            pos = parser.skip_nl(new_pos);
            algebraic_elements.push(type_decl);
        }
        let mut derivations = vec![];
        let (inner_indent, mut pos) = parse_opt_indent(parser, pos);
        while parser.peek(pos, Lexeme::Derive) {
            let (derivation, new_pos) = DeclarationAst::parse_derivation(parser, pos)?;
            pos = parser.skip_nl(new_pos);
            derivations.push(derivation);
        }
        let pos = parse_opt_dedent(parser, pos, inner_indent)?;

        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        let pos = parse_opt_dedent(parser, pos, top_indent)?;

        if derivations.is_empty() {
            Ok(Some((
                DeclarationAst::TypeDecl(type_id, type_args, algebraic_elements, None),
                pos,
            )))
        } else {
            Ok(Some((
                DeclarationAst::TypeDecl(type_id, type_args, algebraic_elements, Some(derivations)),
                pos,
            )))
        }
    }

    pub fn parse_type_alias_decl(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Alias)?;
        let (type_id, pos) = consume_type_id(parser, pos)?;
        let (type_args, pos) = if parser.peek(pos, Lexeme::Assign) {
            (None, pos)
        } else {
            let (args, pos) = DeclarationAst::parse_type_args(parser, pos)?;
            (Some(args), pos)
        };
        let pos = consume_symbol(parser, pos, Lexeme::Assign)?;
        let (base_type, pos) = DeclarationAst::parse_base_type(parser, pos)?;
        Ok(Some((
            DeclarationAst::TypeAlias(type_id, type_args, base_type),
            pos,
        )))
    }

    fn parse_derivation(parser: &'a Parser<'a>, pos: usize) -> Result<(Derivation<'a>, usize)> {
        let pos = consume_symbol(parser, pos, Lexeme::Derive)?;
        if parser.peek(pos, Lexeme::LeftParen) {
            let mut pos = consume_symbol(parser, pos, Lexeme::LeftParen)?;
            let mut traits = vec![];
            while !parser.peek(pos, Lexeme::RightParen) {
                let (trait_id, new_pos) = consume_type_id(parser, pos)?;
                traits.push(trait_id);
                if parser.peek(new_pos, Lexeme::Comma) {
                    pos = consume_symbol(parser, new_pos, Lexeme::Comma)?;
                } else {
                    pos = new_pos;
                }
            }
            pos = consume_symbol(parser, pos, Lexeme::RightParen)?;
            Ok((Derivation::ListOfTraits(traits), pos))
        } else {
            let (trait_id, pos) = consume_type_id(parser, pos)?;
            let pos = parser.skip_nl(pos);
            let pos = consume_symbol(parser, pos, Lexeme::Where)?;
            let pos = parser.skip_nl(pos);
            let pos = consume_symbol(parser, pos, Lexeme::Indent)?;
            let (eq, mut pos) = Equation::parse(parser, pos, false)?;
            let mut eqs = vec![eq];
            pos = parser.skip_nl(pos);
            while !parser.peek(pos, Lexeme::Dedent) {
                let (eq, new_pos) = Equation::parse(parser, pos, false)?;
                pos = parser.skip_nl(new_pos);
                eqs.push(eq);
            }
            let pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
            Ok((Derivation::Trait(trait_id, eqs), pos))
        }
    }

    fn parse_algebraic_type(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(AlgebraicType<'a>, usize)> {
        if parser.peek(pos, Lexeme::Primitive) {
            DeclarationAst::parse_primitive_type(parser, pos)
        } else {
            let (type_id, pos) = consume_type_id(parser, pos)?;
            if parser.peek(pos, Lexeme::LeftCurly) {
                DeclarationAst::parse_record_type(parser, pos, type_id)
            } else {
                let mut params = vec![];
                let mut pos = pos;
                while let Some((alg_elem, new_pos)) = consume_alg_type_param(parser, pos)? {
                    params.push(alg_elem);
                    pos = new_pos;
                }
                if params.is_empty() {
                    Ok((AlgebraicType::Simple(type_id), pos))
                } else {
                    Ok((AlgebraicType::Complex(type_id, params), pos))
                }
            }
        }
    }

    fn parse_primitive_type(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(AlgebraicType<'a>, usize)> {
        let pos = consume_symbol(parser, pos, Lexeme::Primitive)?;
        let (id, pos) = consume_id(parser, pos)?;
        Ok((AlgebraicType::Primitive(id), pos))
    }

    fn parse_record_type(
        parser: &'a Parser<'a>,
        pos: usize,
        type_id: &'a str,
    ) -> Result<(AlgebraicType<'a>, usize)> {
        let (members, pos) = DeclarationAst::parse_record_members(parser, pos)?;
        Ok((AlgebraicType::Record(type_id, members), pos))
    }

    fn parse_record_members(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Vec<RecordElement<'a>>, usize)> {
        let pos = consume_symbol(parser, pos, Lexeme::LeftCurly)?;
        let (in_indent, pos) = parse_opt_indent(parser, pos);
        let (member, mut pos) = DeclarationAst::parse_record_member(parser, pos)?;
        pos = parser.skip_nl(pos);
        let mut members = vec![member];
        while parser.peek(pos, Lexeme::Comma) {
            pos = consume_symbol(parser, pos, Lexeme::Comma)?;
            let (member, new_pos) = DeclarationAst::parse_record_member(parser, pos)?;
            members.push(member);
            pos = new_pos;
        }
        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        let pos = consume_symbol(parser, pos, Lexeme::RightCurly)?;
        Ok((members, pos))
    }

    fn parse_record_member(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(RecordElement<'a>, usize)> {
        let (id, pos) = consume_id(parser, pos)?;
        let pos = consume_symbol(parser, pos, Lexeme::Colon)?;
        let (tid, pos) = consume_type_id(parser, pos)?;
        Ok((RecordElement(id, AlgebraicElement::Type(tid)), pos))
    }

    fn parse_type_args(parser: &'a Parser<'a>, pos: usize) -> Result<(Vec<&'a str>, usize)> {
        let (id, mut pos) = consume_id(parser, pos)?;
        let mut args = vec![id];
        while !parser.peek(pos, Lexeme::Assign) {
            let (id, new_pos) = consume_id(parser, pos)?;
            pos = new_pos;
            args.push(id);
        }
        Ok((args, pos))
    }

    fn parse_base_type(parser: &'a Parser<'a>, pos: usize) -> Result<(BaseType<'a>, usize)> {
        match parser.get_token(pos) {
            Some(Lexeme::LeftParen) => DeclarationAst::parse_tuple(parser, pos),
            Some(Lexeme::LeftCurly) => DeclarationAst::parse_simple_record(parser, pos),
            Some(Lexeme::LeftBracket) => DeclarationAst::parse_array(parser, pos),
            Some(Lexeme::String(_)) => {
                let (str, pos) = consume_string(parser, pos)?;
                Ok((BaseType::ExternType(str), pos))
            }
            _ => {
                let (alg, pos) = DeclarationAst::parse_algebraic_type(parser, pos)?;
                Ok((BaseType::Algebraic(alg), pos))
            }
        }
    }

    fn parse_simple_record(parser: &'a Parser<'a>, pos: usize) -> Result<(BaseType<'a>, usize)> {
        let (rec, pos) = DeclarationAst::parse_record_members(parser, pos)?;
        Ok((BaseType::SimpleRecord(rec), pos))
    }

    fn parse_tuple(parser: &'a Parser<'a>, pos: usize) -> Result<(BaseType<'a>, usize)> {
        let (types, pos) =
            DeclarationAst::parse_seq(parser, pos, Lexeme::LeftParen, Lexeme::RightParen)?;
        Ok((BaseType::Tuple(types), pos))
    }

    fn parse_array(parser: &'a Parser<'a>, pos: usize) -> Result<(BaseType<'a>, usize)> {
        let (types, pos) =
            DeclarationAst::parse_seq(parser, pos, Lexeme::LeftBracket, Lexeme::RightBracket)?;
        Ok((BaseType::Array(types), pos))
    }

    fn parse_seq(
        parser: &'a Parser<'a>,
        pos: usize,
        start: Lexeme,
        end: Lexeme,
    ) -> Result<(Vec<BaseType<'a>>, usize)> {
        let pos = consume_symbol(parser, pos, start)?;
        let (ty, mut pos) = DeclarationAst::parse_base_type(parser, pos)?;
        let mut types = vec![ty];
        while parser.peek(pos, Lexeme::Comma) {
            pos = consume_symbol(parser, pos, Lexeme::Comma)?;
            let (ty, new_pos) = DeclarationAst::parse_base_type(parser, pos)?;
            types.push(ty);
            pos = new_pos;
        }
        let pos = consume_symbol(parser, pos, end)?;
        Ok((types, pos))
    }

    pub(crate) fn parse_trait_decl(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Trait)?;
        let (tid, pos) = consume_type_id(parser, pos)?;
        let (in_indent, mut pos) = parse_opt_indent(parser, pos);
        let mut args = vec![];
        while !parser.peek(pos, Lexeme::Where) {
            let (arg, new_pos) = consume_id(parser, pos)?;
            args.push(arg);
            pos = new_pos;
        }
        let pos = consume_symbol(parser, pos, Lexeme::Where)?;
        let pos = parser.skip_nl(pos);
        let mut pos = consume_symbol(parser, pos, Lexeme::Indent)?;
        let mut trait_decls = vec![];
        while !parser.peek(pos, Lexeme::Dedent) {
            let (name, func_proto, new_pos) = if parser.peek(pos, Lexeme::Effect) {
                DeclarationAst::parse_effect_func_prototype(parser, pos)?
            } else {
                DeclarationAst::parse_func_prototype(parser, pos)?
            };

            trait_decls.push((name, func_proto));
            pos = parser.skip_nl(new_pos);
        }
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
        let pos = parse_opt_dedent(parser, pos, in_indent)?;

        Ok(Some((
            DeclarationAst::TraitDecl(
                tid,
                if args.is_empty() { None } else { Some(args) },
                trait_decls,
            ),
            pos,
        )))
    }

    pub(crate) fn parse_effect_func_prototype(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(&'a str, FuncType<'a>, usize)> {
        let pos = consume_symbol(parser, pos, Lexeme::Effect)?;
        let (func_id, pos) = consume_id(parser, pos)?;
        let pos = consume_symbol(parser, pos, Lexeme::Colon)?;
        let (types, pos) = DeclarationAst::parse_func_types(parser, pos)?;
        Ok((func_id, types, pos))
    }

    pub(crate) fn parse_func_prototype(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(&'a str, FuncType<'a>, usize)> {
        let (func_id, pos) = consume_id(parser, pos)?;
        let pos = consume_symbol(parser, pos, Lexeme::Colon)?;
        let (types, pos) = DeclarationAst::parse_func_types(parser, pos)?;
        Ok((func_id, types, pos))
    }

    fn parse_func_types(parser: &'a Parser<'a>, pos: usize) -> Result<(FuncType<'a>, usize)> {
        let (t, pos) = DeclarationAst::parse_func_type(parser, pos)?;
        if parser.peek(pos, Lexeme::Arrow) {
            let pos = consume_symbol(parser, pos, Lexeme::Arrow)?;
            let (t2, pos) = DeclarationAst::parse_func_types(parser, pos)?;
            Ok((FuncType::Chain(Box::new(t), Box::new(t2)), pos))
        } else {
            Ok((t, pos))
        }
    }

    fn parse_func_type(parser: &'a Parser<'a>, pos: usize) -> Result<(FuncType<'a>, usize)> {
        match parser.get_token(pos) {
            Some(Lexeme::TypeId(_)) => {
                let (type_id, pos) = consume_type_id(parser, pos)?;
                let mut params = vec![];
                let mut pos = pos;
                while let Some((alg_elem, new_pos)) = consume_alg_type_param(parser, pos)? {
                    params.push(alg_elem);
                    pos = new_pos;
                }
                if params.is_empty() {
                    Ok((FuncType::Simple(type_id), pos))
                } else {
                    Ok((FuncType::Complex(type_id, params), pos))
                }
            }
            Some(Lexeme::Id(id)) => Ok((FuncType::Param(id), pos + 1)),
            Some(Lexeme::LeftParen) if parser.peek(pos + 1, Lexeme::RightParen) => {
                Ok((FuncType::Void, pos + 2))
            }
            Some(Lexeme::LeftParen) => {
                let pos = consume_symbol(parser, pos, Lexeme::LeftParen)?;
                let (t, pos) = DeclarationAst::parse_func_types(parser, pos)?;
                let pos = consume_symbol(parser, pos, Lexeme::RightParen)?;
                Ok((t, pos))
            }
            Some(Lexeme::Macro) => Ok((FuncType::Macro, pos + 1)),
            _ => raise_parser_error("expecting a type or a param", parser, pos, true),
        }
    }

    pub(crate) fn parse_extends(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Extends)?;
        let (type_id, pos) = consume_type_id(parser, pos)?;
        let pos = consume_symbol(parser, pos, Lexeme::With)?;
        let (trait_id, pos) = consume_type_id(parser, pos)?;
        let (in_indent, pos) = parse_opt_indent(parser, pos);
        let pos = consume_symbol(parser, pos, Lexeme::Where)?;
        let pos = parser.skip_nl(pos);
        let mut pos = consume_symbol(parser, pos, Lexeme::Indent)?;
        let mut decls = vec![];
        while let Some((decl, new_pos)) = DeclarationAst::parse_func_or_val(parser, pos)? {
            decls.push(decl);
            pos = new_pos;
            if parser.peek(pos, Lexeme::Dedent) {
                break;
            }
        }
        let pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        Ok(Some((
            DeclarationAst::ExtensionDecl(type_id, trait_id, decls),
            pos,
        )))
    }

    pub(crate) fn parse_handler_decl(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Handler)?;
        let (id, pos) = consume_id(parser, pos)?;
        let (args, pos) = Arg::parse(parser, pos)?;
        let pos = consume_symbol(parser, pos, Lexeme::Indent)?;
        let (guards, pos) = Expression::parse_handle_guards(parser, pos)?;
        let pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
        Ok(Some((DeclarationAst::Handler(id, args, guards), pos)))
    }
}

fn consume_alg_type_param<'a>(
    parser: &'a Parser,
    pos: usize,
) -> Result<Option<(AlgebraicElement<'a>, usize)>> {
    match parser.get_token(pos) {
        Some(Lexeme::TypeId(type_id)) => Ok(Some((AlgebraicElement::Type(type_id), pos + 1))),
        Some(Lexeme::Id(id)) => Ok(Some((AlgebraicElement::Param(id), pos + 1))),
        Some(Lexeme::Arrow) => Ok(None),
        Some(Lexeme::NewLine) => Ok(None),
        Some(Lexeme::Indent) => Ok(None),
        Some(Lexeme::Dedent) => Ok(None),
        Some(Lexeme::Guard) => Ok(None),
        Some(Lexeme::RightBracket) => Ok(None),
        Some(Lexeme::RightParen) => Ok(None),
        Some(Lexeme::Comma) => Ok(None),
        Some(Lexeme::Derive) => Ok(None),
        _ => raise_parser_error(
            "Expecting a type or a param, in type declaration",
            parser,
            pos,
            true,
        ),
    }
}
