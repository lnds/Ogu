use crate::lexer::tokens::Token;
use crate::parser::ast::expressions::args::Arg;
use crate::parser::ast::expressions::equations::Equation;
use crate::parser::ast::expressions::expression::{Expression, HandleGuard};
use crate::parser::ast::expressions::guards::Guard;
use crate::parser::{
    consume_id, consume_string, consume_symbol, consume_type_id, look_ahead_where,
    parse_opt_dedent, parse_opt_indent, raise_parser_error, Parser,
};
use anyhow::Result;

#[derive(Debug, Clone)]
pub enum FuncType {
    Void,
    Macro,
    ExternType(String),
    Simple(String),
    Complex(String, Vec<AlgebraicElement>),
    Param(String),
    Chain(Box<FuncType>, Box<FuncType>),
}

#[derive(Debug, Clone)]
pub enum FuncPrototype {
    Normal(String, FuncType),
    Effect(String, FuncType),
}

#[derive(Debug, Clone)]
pub enum AlgebraicElement {
    Type(String),
    Param(String),
}

#[derive(Debug, Clone)]
pub struct RecordElement(String, AlgebraicElement);

#[derive(Debug, Clone)]
pub enum AlgebraicType {
    Simple(String),
    Primitive(String),
    Complex(String, Vec<AlgebraicElement>),
    Record(String, Vec<RecordElement>),
}

#[derive(Debug, Clone)]
pub enum Derivation {
    ListOfTraits(Vec<String>),
    Trait(String, Vec<Equation>),
}

#[derive(Debug, Clone)]
pub enum BaseType {
    Tuple(Vec<BaseType>),
    Array(Vec<BaseType>),
    SimpleRecord(Vec<RecordElement>),
    Algebraic(AlgebraicType),
    ExternType(String),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Value(String, Expression),
    ValueWithWhere(String, Expression, Vec<Equation>),
    Function(String, Vec<Arg>, Expression),
    FunctionWithWhere(String, Vec<Arg>, Expression, Vec<Equation>),
    FunctionWithGuards(String, Vec<Arg>, Vec<Guard>),
    FunctionWithGuardsAndWhere(String, Vec<Arg>, Vec<Guard>, Vec<Equation>),
    TypeDecl(
        String,
        Option<Vec<String>>,
        Vec<AlgebraicType>,
        Option<Vec<Derivation>>,
    ),
    TypeAlias(String, Option<Vec<String>>, BaseType),
    TraitDecl(String, Option<Vec<String>>, Vec<FuncPrototype>),
    ExtensionDecl(String, String, Vec<Declaration>),
    FunctionPrototype(FuncPrototype),
    MacroDecl(Box<Declaration>),
    Effect(FuncPrototype),
    Handler(String, Vec<Arg>, Vec<HandleGuard>),
    DocString(Option<String>),
}

type DeclVec = Vec<Declaration>;

#[derive(Debug)]
pub struct Body {
    declarations: Vec<Declaration>,
}

type DeclParseResult = Result<Option<(Declaration, usize)>>;

impl Body {
    pub(crate) fn parse(parser: &Parser, pos: usize) -> Result<Self> {
        let declarations = Body::parse_decls(parser, pos)?;
        Ok(Body { declarations })
    }

    fn parse_decls(parser: &Parser, pos: usize) -> Result<DeclVec> {
        let mut result = vec![];
        let mut pos = pos;
        while let Some((decl, new_pos)) = Body::parse_decl(parser, pos)? {
            result.push(decl);
            pos = new_pos;
        }
        Ok(result)
    }

    pub fn parse_decl(parser: &Parser, pos: usize) -> DeclParseResult {
        let pos = parser.skip_nl(pos);
        match parser.get_token(pos) {
            None => Ok(None),
            Some(Token::Macro) => {
                let pos = consume_symbol(parser, pos, Token::Macro)?;
                if let Some((decl, pos)) = Body::parse_decl(parser, pos)? {
                    Ok(Some((Declaration::MacroDecl(Box::new(decl)), pos)))
                } else {
                    raise_parser_error("expecting macro declaration", parser, pos, false)
                }
            }
            Some(Token::Id(_)) if parser.peek(pos + 1, Token::Colon) => {
                let (proto, pos) = Declaration::parse_func_prototype(parser, pos)?;
                Ok(Some((Declaration::FunctionPrototype(proto), pos)))
            }
            Some(Token::Id(_)) => Declaration::parse_func_or_val(parser, pos),
            Some(Token::Type) => Declaration::parse_type_decl(parser, pos),
            Some(Token::Alias) => Declaration::parse_type_alias_decl(parser, pos),
            Some(Token::Trait) => Declaration::parse_trait_decl(parser, pos),
            Some(Token::Handler) => Declaration::parse_handler_decl(parser, pos),
            Some(Token::Effect) => {
                let (prot, pos) = Declaration::parse_effect_func_prototype(parser, pos)?;
                Ok(Some((Declaration::Effect(prot), pos)))
            }
            Some(Token::Extends) => Declaration::parse_extends(parser, pos),
            Some(Token::LargeString(i)) => Ok(Some((
                Declaration::DocString(parser.get_large_string(i)),
                pos + 1,
            ))),
            _ => raise_parser_error("expecting a declaration", parser, pos, true),
        }
    }
}

impl Declaration {
    pub fn parse_func_or_val(parser: &Parser, pos: usize) -> DeclParseResult {
        let (eq, pos) = Equation::parse(parser, pos, true)?;
        let (opt_where, pos) = if let Some(where_pos) = look_ahead_where(parser, pos) {
            let (where_decl, mut pos) = Declaration::parse_where(parser, where_pos)?;
            if parser.peek(pos, Token::Dedent) {
                pos = consume_symbol(parser, pos, Token::Dedent)?;
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

    fn parse_where(parser: &Parser, pos: usize) -> Result<(Vec<Equation>, usize)> {
        let pos = consume_symbol(parser, pos, Token::Where)?;
        let (indent, mut pos) = parse_opt_indent(parser, pos);
        let mut eqs = vec![];
        if indent {
            while !parser.peek(pos, Token::Dedent) {
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

impl Declaration {
    fn parse_type_decl(parser: &Parser, pos: usize) -> DeclParseResult {
        let pos = consume_symbol(parser, pos, Token::Type)?;
        if parser.peek(pos, Token::Alias) {
            return Declaration::parse_type_alias_decl(parser, pos);
        }

        let (type_id, pos) = consume_type_id(parser, pos)?;
        let (type_args, pos) =
            if parser.peek(pos, Token::Assign) || parser.peek(pos, Token::NewLine) {
                (None, pos)
            } else {
                let (args, pos) = Declaration::parse_type_args(parser, pos)?;
                (Some(args), pos)
            };
        let (top_indent, pos) = parse_opt_indent(parser, pos);
        let pos = consume_symbol(parser, pos, Token::Assign)?;
        let (type_decl, pos) = Declaration::parse_algebraic_type(parser, pos)?;
        let mut algebraic_elements = vec![type_decl];
        let (in_indent, mut pos) = parse_opt_indent(parser, pos);
        while parser.peek(pos, Token::Guard) {
            let new_pos = consume_symbol(parser, pos, Token::Guard)?;
            let (type_decl, new_pos) = Declaration::parse_algebraic_type(parser, new_pos)?;
            pos = parser.skip_nl(new_pos);
            algebraic_elements.push(type_decl);
        }
        let mut derivations = vec![];
        let (inner_indent, mut pos) = parse_opt_indent(parser, pos);
        while parser.peek(pos, Token::Derive) {
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

    pub fn parse_type_alias_decl(parser: &Parser, pos: usize) -> DeclParseResult {
        let pos = consume_symbol(parser, pos, Token::Alias)?;
        let (type_id, pos) = consume_type_id(parser, pos)?;
        let (type_args, pos) = if parser.peek(pos, Token::Assign) {
            (None, pos)
        } else {
            let (args, pos) = Declaration::parse_type_args(parser, pos)?;
            (Some(args), pos)
        };
        let pos = consume_symbol(parser, pos, Token::Assign)?;
        let (base_type, pos) = Declaration::parse_base_type(parser, pos)?;
        Ok(Some((
            Declaration::TypeAlias(type_id, type_args, base_type),
            pos,
        )))
    }

    fn parse_derivation(parser: &Parser, pos: usize) -> Result<(Derivation, usize)> {
        let pos = consume_symbol(parser, pos, Token::Derive)?;
        if parser.peek(pos, Token::LeftParen) {
            let mut pos = consume_symbol(parser, pos, Token::LeftParen)?;
            let mut traits = vec![];
            while !parser.peek(pos, Token::RightParen) {
                let (trait_id, new_pos) = consume_type_id(parser, pos)?;
                traits.push(trait_id);
                if parser.peek(new_pos, Token::Comma) {
                    pos = consume_symbol(parser, new_pos, Token::Comma)?;
                } else {
                    pos = new_pos;
                }
            }
            pos = consume_symbol(parser, pos, Token::RightParen)?;
            Ok((Derivation::ListOfTraits(traits), pos))
        } else {
            let (trait_id, pos) = consume_type_id(parser, pos)?;
            let pos = parser.skip_nl(pos);
            let pos = consume_symbol(parser, pos, Token::Where)?;
            let pos = parser.skip_nl(pos);
            let pos = consume_symbol(parser, pos, Token::Indent)?;
            let (eq, mut pos) = Equation::parse(parser, pos, false)?;
            let mut eqs = vec![eq];
            pos = parser.skip_nl(pos);
            while !parser.peek(pos, Token::Dedent) {
                let (eq, new_pos) = Equation::parse(parser, pos, false)?;
                pos = parser.skip_nl(new_pos);
                eqs.push(eq);
            }
            let pos = consume_symbol(parser, pos, Token::Dedent)?;
            Ok((Derivation::Trait(trait_id, eqs), pos))
        }
    }

    fn parse_algebraic_type(parser: &Parser, pos: usize) -> Result<(AlgebraicType, usize)> {
        if parser.peek(pos, Token::Primitive) {
            Declaration::parse_primitive_type(parser, pos)
        } else {
            let (type_id, pos) = consume_type_id(parser, pos)?;
            if parser.peek(pos, Token::LeftCurly) {
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

    fn parse_primitive_type(parser: &Parser, pos: usize) -> Result<(AlgebraicType, usize)> {
        let pos = consume_symbol(parser, pos, Token::Primitive)?;
        let (id, pos) = consume_id(parser, pos)?;
        Ok((AlgebraicType::Primitive(id), pos))
    }

    fn parse_record_type(
        parser: &Parser,
        pos: usize,
        type_id: String,
    ) -> Result<(AlgebraicType, usize)> {
        let (members, pos) = Declaration::parse_record_members(parser, pos)?;
        Ok((AlgebraicType::Record(type_id, members), pos))
    }

    fn parse_record_members(parser: &Parser, pos: usize) -> Result<(Vec<RecordElement>, usize)> {
        let pos = consume_symbol(parser, pos, Token::LeftCurly)?;
        let (in_indent, pos) = parse_opt_indent(parser, pos);
        let (member, mut pos) = Declaration::parse_record_member(parser, pos)?;
        pos = parser.skip_nl(pos);
        let mut members = vec![member];
        while parser.peek(pos, Token::Comma) {
            pos = consume_symbol(parser, pos, Token::Comma)?;
            let (member, new_pos) = Declaration::parse_record_member(parser, pos)?;
            members.push(member);
            pos = new_pos;
        }
        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        let pos = consume_symbol(parser, pos, Token::RightCurly)?;
        Ok((members, pos))
    }

    fn parse_record_member(parser: &Parser, pos: usize) -> Result<(RecordElement, usize)> {
        let (id, pos) = consume_id(parser, pos)?;
        let pos = consume_symbol(parser, pos, Token::Colon)?;
        let (tid, pos) = consume_type_id(parser, pos)?;
        Ok((RecordElement(id, AlgebraicElement::Type(tid)), pos))
    }

    fn parse_type_args(parser: &Parser, pos: usize) -> Result<(Vec<String>, usize)> {
        let (id, mut pos) = consume_id(parser, pos)?;
        let mut args = vec![id];
        while !parser.peek(pos, Token::Assign) {
            let (id, new_pos) = consume_id(parser, pos)?;
            pos = new_pos;
            args.push(id);
        }
        Ok((args, pos))
    }
}

impl Declaration {
    fn parse_trait_decl(parser: &Parser, pos: usize) -> DeclParseResult {
        let pos = consume_symbol(parser, pos, Token::Trait)?;
        let (tid, pos) = consume_type_id(parser, pos)?;
        let (in_indent, mut pos) = parse_opt_indent(parser, pos);
        let mut args = vec![];
        while !parser.peek(pos, Token::Where) {
            let (arg, new_pos) = consume_id(parser, pos)?;
            args.push(arg);
            pos = new_pos;
        }
        let pos = consume_symbol(parser, pos, Token::Where)?;
        let pos = parser.skip_nl(pos);
        let mut pos = consume_symbol(parser, pos, Token::Indent)?;
        let mut trait_decls = vec![];
        while !parser.peek(pos, Token::Dedent) {
            let (func_proto, new_pos) = if parser.peek(pos, Token::Effect) {
                Declaration::parse_effect_func_prototype(parser, pos)?
            } else {
                Declaration::parse_func_prototype(parser, pos)?
            };

            trait_decls.push(func_proto);
            pos = parser.skip_nl(new_pos);
        }
        let pos = parser.skip_nl(pos);
        let pos = consume_symbol(parser, pos, Token::Dedent)?;
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
    fn parse_effect_func_prototype(parser: &Parser, pos: usize) -> Result<(FuncPrototype, usize)> {
        let pos = consume_symbol(parser, pos, Token::Effect)?;
        let (func_id, pos) = consume_id(parser, pos)?;
        let pos = consume_symbol(parser, pos, Token::Colon)?;
        let (types, pos) = Declaration::parse_func_types(parser, pos)?;
        Ok((FuncPrototype::Effect(func_id, types), pos))
    }

    fn parse_func_prototype(parser: &Parser, pos: usize) -> Result<(FuncPrototype, usize)> {
        let (func_id, pos) = consume_id(parser, pos)?;
        let pos = consume_symbol(parser, pos, Token::Colon)?;
        let (types, pos) = Declaration::parse_func_types(parser, pos)?;
        Ok((FuncPrototype::Normal(func_id, types), pos))
    }

    fn parse_func_types(parser: &Parser, pos: usize) -> Result<(FuncType, usize)> {
        let (t, pos) = Declaration::parse_func_type(parser, pos)?;
        if parser.peek(pos, Token::Arrow) {
            let pos = consume_symbol(parser, pos, Token::Arrow)?;
            let (t2, pos) = Declaration::parse_func_types(parser, pos)?;
            Ok((FuncType::Chain(Box::new(t), Box::new(t2)), pos))
        } else {
            Ok((t, pos))
        }
    }

    fn parse_func_type(parser: &Parser, pos: usize) -> Result<(FuncType, usize)> {
        match parser.get_token(pos) {
            Some(Token::TypeId(_)) => {
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
            Some(Token::Id(id)) => Ok((FuncType::Param(id.to_string()), pos + 1)),
            Some(Token::LeftParen) if parser.peek(pos + 1, Token::RightParen) => {
                Ok((FuncType::Void, pos + 2))
            }
            Some(Token::LeftParen) => {
                let pos = consume_symbol(parser, pos, Token::LeftParen)?;
                let (t, pos) = Declaration::parse_func_types(parser, pos)?;
                let pos = consume_symbol(parser, pos, Token::RightParen)?;
                Ok((t, pos))
            }
            Some(Token::Macro) => Ok((FuncType::Macro, pos + 1)),
            _ => raise_parser_error("expecting a type or a param", parser, pos, true),
        }
    }
}

impl Declaration {
    fn parse_extends(parser: &Parser, pos: usize) -> DeclParseResult {
        let pos = consume_symbol(parser, pos, Token::Extends)?;
        let (type_id, pos) = consume_type_id(parser, pos)?;
        let pos = consume_symbol(parser, pos, Token::With)?;
        let (trait_id, pos) = consume_type_id(parser, pos)?;
        let (in_indent, pos) = parse_opt_indent(parser, pos);
        let pos = consume_symbol(parser, pos, Token::Where)?;
        let pos = parser.skip_nl(pos);
        let mut pos = consume_symbol(parser, pos, Token::Indent)?;
        let mut decls = vec![];
        while let Some((decl, new_pos)) = Declaration::parse_func_or_val(parser, pos)? {
            decls.push(decl);
            pos = new_pos;
            if parser.peek(pos, Token::Dedent) {
                break;
            }
        }
        let pos = consume_symbol(parser, pos, Token::Dedent)?;
        let pos = parse_opt_dedent(parser, pos, in_indent)?;
        Ok(Some((
            Declaration::ExtensionDecl(type_id, trait_id, decls),
            pos,
        )))
    }
}

impl Declaration {
    fn parse_base_type(parser: &Parser, pos: usize) -> Result<(BaseType, usize)> {
        match parser.get_token(pos) {
            Some(Token::LeftParen) => Declaration::parse_tuple(parser, pos),
            Some(Token::LeftCurly) => Declaration::parse_simple_record(parser, pos),
            Some(Token::LeftBracket) => Declaration::parse_array(parser, pos),
            Some(Token::String(_)) => {
                let (str, pos) = consume_string(parser, pos)?;
                Ok((BaseType::ExternType(str), pos))
            }
            _ => {
                let (alg, pos) = Declaration::parse_algebraic_type(parser, pos)?;
                Ok((BaseType::Algebraic(alg), pos))
            }
        }
    }

    fn parse_simple_record(parser: &Parser, pos: usize) -> Result<(BaseType, usize)> {
        let (rec, pos) = Declaration::parse_record_members(parser, pos)?;
        Ok((BaseType::SimpleRecord(rec), pos))
    }

    fn parse_tuple(parser: &Parser, pos: usize) -> Result<(BaseType, usize)> {
        let (types, pos) =
            Declaration::parse_seq(parser, pos, Token::LeftParen, Token::RightParen)?;
        Ok((BaseType::Tuple(types), pos))
    }

    fn parse_array(parser: &Parser, pos: usize) -> Result<(BaseType, usize)> {
        let (types, pos) =
            Declaration::parse_seq(parser, pos, Token::LeftBracket, Token::RightBracket)?;
        Ok((BaseType::Array(types), pos))
    }

    fn parse_seq(
        parser: &Parser,
        pos: usize,
        start: Token,
        end: Token,
    ) -> Result<(Vec<BaseType>, usize)> {
        let pos = consume_symbol(parser, pos, start)?;
        let (ty, mut pos) = Declaration::parse_base_type(parser, pos)?;
        let mut types = vec![ty];
        while parser.peek(pos, Token::Comma) {
            pos = consume_symbol(parser, pos, Token::Comma)?;
            let (ty, new_pos) = Declaration::parse_base_type(parser, pos)?;
            types.push(ty);
            pos = new_pos;
        }
        let pos = consume_symbol(parser, pos, end)?;
        Ok((types, pos))
    }
}

impl Declaration {
    fn parse_handler_decl(parser: &Parser, pos: usize) -> DeclParseResult {
        let pos = consume_symbol(parser, pos, Token::Handler)?;
        let (id, pos) = consume_id(parser, pos)?;
        let (args, pos) = Arg::parse(parser, pos)?;
        let pos = consume_symbol(parser, pos, Token::Indent)?;
        let (guards, pos) = Expression::parse_handle_guards(parser, pos)?;
        let pos = consume_symbol(parser, pos, Token::Dedent)?;
        Ok(Some((Declaration::Handler(id, args, guards), pos)))
    }
}

fn consume_alg_type_param(
    parser: &Parser,
    pos: usize,
) -> Result<Option<(AlgebraicElement, usize)>> {
    match parser.get_token(pos) {
        Some(Token::TypeId(type_id)) => {
            Ok(Some((AlgebraicElement::Type(type_id.to_string()), pos + 1)))
        }
        Some(Token::Id(id)) => Ok(Some((AlgebraicElement::Param(id.to_string()), pos + 1))),
        Some(Token::Arrow) => Ok(None),
        Some(Token::NewLine) => Ok(None),
        Some(Token::Indent) => Ok(None),
        Some(Token::Dedent) => Ok(None),
        Some(Token::Guard) => Ok(None),
        Some(Token::RightBracket) => Ok(None),
        Some(Token::RightParen) => Ok(None),
        Some(Token::Comma) => Ok(None),
        Some(Token::Derive) => Ok(None),
        _ => raise_parser_error(
            "Expecting a type or a param, in type declaration",
            parser,
            pos,
            true,
        ),
    }
}
