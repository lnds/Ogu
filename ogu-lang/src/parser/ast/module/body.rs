use crate::lexer::tokens::Lexeme;
use crate::parser::ast::expressions::args::{Arg, Args};
use crate::parser::ast::expressions::equations::Equation;
use crate::parser::ast::expressions::expression::{Expression, HandleGuard};
use crate::parser::ast::expressions::guards::Guard;
use crate::parser::{
    consume_id, consume_string, consume_symbol, consume_type_id, look_ahead_where,
    parse_opt_dedent, parse_opt_indent, raise_parser_error, Parser,
};
use anyhow::Result;

#[derive(Debug, Clone)]
pub(crate) enum FuncType<'a> {
    Void,
    Macro,
    Simple(&'a str),
    Complex(&'a str, Vec<AlgebraicElement<'a>>),
    Param(&'a str),
    Chain(Box<FuncType<'a>>, Box<FuncType<'a>>),
}

#[derive(Debug, Clone)]
pub(crate) enum FuncPrototype<'a> {
    Normal(&'a str, FuncType<'a>),
    Effect(&'a str, FuncType<'a>),
}

impl<'a> FuncPrototype<'a> {
    fn get_name(&self) -> &'a str {
        match self {
            FuncPrototype::Normal(s, _) => s,
            FuncPrototype::Effect(s, _) => s,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum AlgebraicElement<'a> {
    Type(&'a str),
    Param(&'a str),
}

#[derive(Debug, Clone)]
pub(crate) struct RecordElement<'a>(&'a str, AlgebraicElement<'a>);

#[derive(Debug, Clone)]
pub(crate) enum AlgebraicType<'a> {
    Simple(&'a str),
    Primitive(&'a str),
    Complex(&'a str, Vec<AlgebraicElement<'a>>),
    Record(&'a str, Vec<RecordElement<'a>>),
}

#[derive(Debug, Clone)]
pub(crate) enum Derivation<'a> {
    ListOfTraits(Vec<&'a str>),
    Trait(&'a str, Vec<Equation<'a>>),
}

#[derive(Debug, Clone)]
pub(crate) enum BaseType<'a> {
    Tuple(Vec<BaseType<'a>>),
    Array(Vec<BaseType<'a>>),
    SimpleRecord(Vec<RecordElement<'a>>),
    Algebraic(AlgebraicType<'a>),
    ExternType(&'a str),
}

#[derive(Debug, Clone)]
pub(crate) enum Declaration<'a> {
    Value(&'a str, Expression<'a>),
    ValueWithWhere(&'a str, Expression<'a>, Vec<Equation<'a>>),
    Function(&'a str, Args<'a>, Expression<'a>),
    FunctionWithWhere(&'a str, Args<'a>, Expression<'a>, Vec<Equation<'a>>),
    FunctionWithGuards(&'a str, Args<'a>, Vec<Guard<'a>>),
    FunctionWithGuardsAndWhere(&'a str, Args<'a>, Vec<Guard<'a>>, Vec<Equation<'a>>),
    TypeDecl(
        &'a str,
        Option<Vec<&'a str>>,
        Vec<AlgebraicType<'a>>,
        Option<Vec<Derivation<'a>>>,
    ),
    TypeAlias(&'a str, Option<Vec<&'a str>>, BaseType<'a>),
    TraitDecl(&'a str, Option<Vec<&'a str>>, Vec<FuncPrototype<'a>>),
    ExtensionDecl(&'a str, &'a str, Vec<Declaration<'a>>),
    FunctionPrototype(FuncPrototype<'a>),
    MacroDecl(Box<Declaration<'a>>),
    Effect(FuncPrototype<'a>),
    Handler(&'a str, Args<'a>, Vec<HandleGuard<'a>>),
    DocString(Option<String>),
}

impl<'a> Declaration<'a> {
    pub fn get_name(&self) -> &'a str {
        match self {
            Declaration::Value(val, _) => val,
            Declaration::ValueWithWhere(val, _, _) => val,
            Declaration::Function(f, _, _) => f,
            Declaration::FunctionWithWhere(f, _, _, _) => f,
            Declaration::FunctionWithGuards(f, _, _) => f,
            Declaration::FunctionWithGuardsAndWhere(f, _, _, _) => f,
            Declaration::TypeDecl(ty, _, _, _) => ty,
            Declaration::TypeAlias(ty, _, _) => ty,
            Declaration::TraitDecl(tr, _, _) => tr,
            Declaration::ExtensionDecl(e, _, _) => e,
            Declaration::FunctionPrototype(ft) => ft.get_name(),
            Declaration::MacroDecl(d) => d.get_name(),
            Declaration::Effect(fp) => fp.get_name(),
            Declaration::Handler(h, _, _) => h,
            Declaration::DocString(Some(_)) => "",
            Declaration::DocString(None) => "",
        }
    }
}

type DeclVec<'a> = Vec<Declaration<'a>>;

#[derive(Debug, Clone)]
pub(crate) struct BodyAst<'a> {
    pub(crate) declarations: Vec<Declaration<'a>>,
}

type DeclParseResult<'a> = Result<Option<(Declaration<'a>, usize)>>;

impl<'a> BodyAst<'a> {
    pub(crate) fn get_decls(self) -> Vec<Declaration<'a>> {
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
        Ok(result)
    }

    pub fn parse_decl(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
        let pos = parser.skip_nl(pos);
        match parser.get_token(pos) {
            None => Ok(None),
            Some(Lexeme::Macro) => {
                let pos = consume_symbol(parser, pos, Lexeme::Macro)?;
                if let Some((decl, pos)) = BodyAst::parse_decl(parser, pos)? {
                    Ok(Some((Declaration::MacroDecl(Box::new(decl)), pos)))
                } else {
                    raise_parser_error("expecting macro declaration", parser, pos, false)
                }
            }
            Some(Lexeme::Id(_)) if parser.peek(pos + 1, Lexeme::Colon) => {
                let (proto, pos) = Declaration::parse_func_prototype(parser, pos)?;
                Ok(Some((Declaration::FunctionPrototype(proto), pos)))
            }
            Some(Lexeme::Id(_)) => Declaration::parse_func_or_val(parser, pos),
            Some(Lexeme::Type) => Declaration::parse_type_decl(parser, pos),
            Some(Lexeme::Alias) => Declaration::parse_type_alias_decl(parser, pos),
            Some(Lexeme::Trait) => Declaration::parse_trait_decl(parser, pos),
            Some(Lexeme::Handler) => Declaration::parse_handler_decl(parser, pos),
            Some(Lexeme::Effect) => {
                let (prot, pos) = Declaration::parse_effect_func_prototype(parser, pos)?;
                Ok(Some((Declaration::Effect(prot), pos)))
            }
            Some(Lexeme::Extends) => Declaration::parse_extends(parser, pos),
            Some(Lexeme::LargeString(i)) => Ok(Some((
                Declaration::DocString(parser.get_large_string(i)),
                pos + 1,
            ))),
            _ => raise_parser_error("expecting a declaration", parser, pos, true),
        }
    }
}

impl<'a> Declaration<'a> {
    pub fn parse_func_or_val(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
        let (eq, pos) = Equation::parse(parser, pos, true)?;
        let (opt_where, pos) = if let Some(where_pos) = look_ahead_where(parser, pos) {
            let (where_decl, mut pos) = Declaration::parse_where(parser, where_pos)?;
            if parser.peek(pos, Lexeme::Dedent) {
                pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
            }
            (Some(where_decl), pos)
        } else {
            (None, pos)
        };
        match eq {
            Equation::Value(name, expr) => {
                if let Some(where_decl) = opt_where {
                    Ok(Some((
                        Declaration::ValueWithWhere(name, expr, where_decl),
                        pos,
                    )))
                } else {
                    Ok(Some((Declaration::Value(name, expr), pos)))
                }
            }
            Equation::Function(name, args, expr) => {
                if let Some(where_decl) = opt_where {
                    Ok(Some((
                        Declaration::FunctionWithWhere(name, args, expr, where_decl),
                        pos,
                    )))
                } else {
                    Ok(Some((Declaration::Function(name, args, expr), pos)))
                }
            }
            Equation::FunctionWithGuards(name, args, guards) => {
                if let Some(where_decl) = opt_where {
                    Ok(Some((
                        Declaration::FunctionWithGuardsAndWhere(name, args, guards, where_decl),
                        pos,
                    )))
                } else {
                    Ok(Some((
                        Declaration::FunctionWithGuards(name, args, guards),
                        pos,
                    )))
                }
            }
            _ => raise_parser_error("invalid declaration", parser, pos, true),
        }
    }

    fn parse_where(parser: &'a Parser<'a>, pos: usize) -> Result<(Vec<Equation<'a>>, usize)> {
        let pos = consume_symbol(parser, pos, Lexeme::Where)?;
        let (indent, mut pos) = parse_opt_indent(parser, pos);
        let mut eqs = vec![];
        if indent {
            while !parser.peek(pos, Lexeme::Dedent) {
                pos = parser.skip_nl(pos);
                let (eq, new_pos) = Equation::parse(parser, pos, true)?;
                eqs.push(eq);
                pos = parser.skip_nl(new_pos);
            }
        } else {
            let (eq, new_pos) = Equation::parse(parser, pos, true)?;
            eqs.push(eq);
            pos = new_pos;
        }
        let pos = parse_opt_dedent(parser, pos, indent)?;
        Ok((eqs, pos))
    }
}

impl<'a> Declaration<'a> {
    fn parse_type_decl(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Type)?;
        if parser.peek(pos, Lexeme::Alias) {
            return Declaration::parse_type_alias_decl(parser, pos);
        }

        let (type_id, pos) = consume_type_id(parser, pos)?;
        let (type_args, pos) =
            if parser.peek(pos, Lexeme::Assign) || parser.peek(pos, Lexeme::NewLine) {
                (None, pos)
            } else {
                let (args, pos) = Declaration::parse_type_args(parser, pos)?;
                (Some(args), pos)
            };
        let (top_indent, pos) = parse_opt_indent(parser, pos);
        let pos = consume_symbol(parser, pos, Lexeme::Assign)?;
        let (type_decl, pos) = Declaration::parse_algebraic_type(parser, pos)?;
        let mut algebraic_elements = vec![type_decl];
        let (in_indent, mut pos) = parse_opt_indent(parser, pos);
        while parser.peek(pos, Lexeme::Guard) {
            let new_pos = consume_symbol(parser, pos, Lexeme::Guard)?;
            let (type_decl, new_pos) = Declaration::parse_algebraic_type(parser, new_pos)?;
            pos = parser.skip_nl(new_pos);
            algebraic_elements.push(type_decl);
        }
        let mut derivations = vec![];
        let (inner_indent, mut pos) = parse_opt_indent(parser, pos);
        while parser.peek(pos, Lexeme::Derive) {
            let (derivation, new_pos) = Declaration::parse_derivation(parser, pos)?;
            pos = parser.skip_nl(new_pos);
            derivations.push(derivation);
        }
        let pos = parse_opt_dedent(parser, pos, inner_indent)?;

        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        let pos = parse_opt_dedent(parser, pos, top_indent)?;

        if derivations.is_empty() {
            Ok(Some((
                Declaration::TypeDecl(type_id, type_args, algebraic_elements, None),
                pos,
            )))
        } else {
            Ok(Some((
                Declaration::TypeDecl(type_id, type_args, algebraic_elements, Some(derivations)),
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
            let (args, pos) = Declaration::parse_type_args(parser, pos)?;
            (Some(args), pos)
        };
        let pos = consume_symbol(parser, pos, Lexeme::Assign)?;
        let (base_type, pos) = Declaration::parse_base_type(parser, pos)?;
        Ok(Some((
            Declaration::TypeAlias(type_id, type_args, base_type),
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
            Declaration::parse_primitive_type(parser, pos)
        } else {
            let (type_id, pos) = consume_type_id(parser, pos)?;
            if parser.peek(pos, Lexeme::LeftCurly) {
                Declaration::parse_record_type(parser, pos, type_id)
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
        let (members, pos) = Declaration::parse_record_members(parser, pos)?;
        Ok((AlgebraicType::Record(type_id, members), pos))
    }

    fn parse_record_members(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(Vec<RecordElement<'a>>, usize)> {
        let pos = consume_symbol(parser, pos, Lexeme::LeftCurly)?;
        let (in_indent, pos) = parse_opt_indent(parser, pos);
        let (member, mut pos) = Declaration::parse_record_member(parser, pos)?;
        pos = parser.skip_nl(pos);
        let mut members = vec![member];
        while parser.peek(pos, Lexeme::Comma) {
            pos = consume_symbol(parser, pos, Lexeme::Comma)?;
            let (member, new_pos) = Declaration::parse_record_member(parser, pos)?;
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
}

impl<'a> Declaration<'a> {
    fn parse_trait_decl(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
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
            let (func_proto, new_pos) = if parser.peek(pos, Lexeme::Effect) {
                Declaration::parse_effect_func_prototype(parser, pos)?
            } else {
                Declaration::parse_func_prototype(parser, pos)?
            };

            trait_decls.push(func_proto);
            pos = parser.skip_nl(new_pos);
        }
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
        let pos = parse_opt_dedent(parser, pos, in_indent)?;

        Ok(Some((
            Declaration::TraitDecl(
                tid,
                if args.is_empty() { None } else { Some(args) },
                trait_decls,
            ),
            pos,
        )))
    }

    fn parse_effect_func_prototype(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(FuncPrototype<'a>, usize)> {
        let pos = consume_symbol(parser, pos, Lexeme::Effect)?;
        let (func_id, pos) = consume_id(parser, pos)?;
        let pos = consume_symbol(parser, pos, Lexeme::Colon)?;
        let (types, pos) = Declaration::parse_func_types(parser, pos)?;
        Ok((FuncPrototype::Effect(func_id, types), pos))
    }

    fn parse_func_prototype(
        parser: &'a Parser<'a>,
        pos: usize,
    ) -> Result<(FuncPrototype<'a>, usize)> {
        let (func_id, pos) = consume_id(parser, pos)?;
        let pos = consume_symbol(parser, pos, Lexeme::Colon)?;
        let (types, pos) = Declaration::parse_func_types(parser, pos)?;
        Ok((FuncPrototype::Normal(func_id, types), pos))
    }

    fn parse_func_types(parser: &'a Parser<'a>, pos: usize) -> Result<(FuncType<'a>, usize)> {
        let (t, pos) = Declaration::parse_func_type(parser, pos)?;
        if parser.peek(pos, Lexeme::Arrow) {
            let pos = consume_symbol(parser, pos, Lexeme::Arrow)?;
            let (t2, pos) = Declaration::parse_func_types(parser, pos)?;
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
                let (t, pos) = Declaration::parse_func_types(parser, pos)?;
                let pos = consume_symbol(parser, pos, Lexeme::RightParen)?;
                Ok((t, pos))
            }
            Some(Lexeme::Macro) => Ok((FuncType::Macro, pos + 1)),
            _ => raise_parser_error("expecting a type or a param", parser, pos, true),
        }
    }
}

impl<'a> Declaration<'a> {
    fn parse_extends(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Extends)?;
        let (type_id, pos) = consume_type_id(parser, pos)?;
        let pos = consume_symbol(parser, pos, Lexeme::With)?;
        let (trait_id, pos) = consume_type_id(parser, pos)?;
        let (in_indent, pos) = parse_opt_indent(parser, pos);
        let pos = consume_symbol(parser, pos, Lexeme::Where)?;
        let pos = parser.skip_nl(pos);
        let mut pos = consume_symbol(parser, pos, Lexeme::Indent)?;
        let mut decls = vec![];
        while let Some((decl, new_pos)) = Declaration::parse_func_or_val(parser, pos)? {
            decls.push(decl);
            pos = new_pos;
            if parser.peek(pos, Lexeme::Dedent) {
                break;
            }
        }
        let pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        Ok(Some((
            Declaration::ExtensionDecl(type_id, trait_id, decls),
            pos,
        )))
    }
}

impl<'a> Declaration<'a> {
    fn parse_base_type(parser: &'a Parser<'a>, pos: usize) -> Result<(BaseType<'a>, usize)> {
        match parser.get_token(pos) {
            Some(Lexeme::LeftParen) => Declaration::parse_tuple(parser, pos),
            Some(Lexeme::LeftCurly) => Declaration::parse_simple_record(parser, pos),
            Some(Lexeme::LeftBracket) => Declaration::parse_array(parser, pos),
            Some(Lexeme::String(_)) => {
                let (str, pos) = consume_string(parser, pos)?;
                Ok((BaseType::ExternType(str), pos))
            }
            _ => {
                let (alg, pos) = Declaration::parse_algebraic_type(parser, pos)?;
                Ok((BaseType::Algebraic(alg), pos))
            }
        }
    }

    fn parse_simple_record(parser: &'a Parser<'a>, pos: usize) -> Result<(BaseType<'a>, usize)> {
        let (rec, pos) = Declaration::parse_record_members(parser, pos)?;
        Ok((BaseType::SimpleRecord(rec), pos))
    }

    fn parse_tuple(parser: &'a Parser<'a>, pos: usize) -> Result<(BaseType<'a>, usize)> {
        let (types, pos) =
            Declaration::parse_seq(parser, pos, Lexeme::LeftParen, Lexeme::RightParen)?;
        Ok((BaseType::Tuple(types), pos))
    }

    fn parse_array(parser: &'a Parser<'a>, pos: usize) -> Result<(BaseType<'a>, usize)> {
        let (types, pos) =
            Declaration::parse_seq(parser, pos, Lexeme::LeftBracket, Lexeme::RightBracket)?;
        Ok((BaseType::Array(types), pos))
    }

    fn parse_seq(
        parser: &'a Parser<'a>,
        pos: usize,
        start: Lexeme,
        end: Lexeme,
    ) -> Result<(Vec<BaseType<'a>>, usize)> {
        let pos = consume_symbol(parser, pos, start)?;
        let (ty, mut pos) = Declaration::parse_base_type(parser, pos)?;
        let mut types = vec![ty];
        while parser.peek(pos, Lexeme::Comma) {
            pos = consume_symbol(parser, pos, Lexeme::Comma)?;
            let (ty, new_pos) = Declaration::parse_base_type(parser, pos)?;
            types.push(ty);
            pos = new_pos;
        }
        let pos = consume_symbol(parser, pos, end)?;
        Ok((types, pos))
    }
}

impl<'a> Declaration<'a> {
    fn parse_handler_decl(parser: &'a Parser<'a>, pos: usize) -> DeclParseResult<'a> {
        let pos = consume_symbol(parser, pos, Lexeme::Handler)?;
        let (id, pos) = consume_id(parser, pos)?;
        let (args, pos) = Arg::parse(parser, pos)?;
        let pos = consume_symbol(parser, pos, Lexeme::Indent)?;
        let (guards, pos) = Expression::parse_handle_guards(parser, pos)?;
        let pos = consume_symbol(parser, pos, Lexeme::Dedent)?;
        Ok(Some((Declaration::Handler(id, args, guards), pos)))
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