use anyhow::Result;

use crate::parser::ast::expressions::args::Args;
use crate::parser::ast::expressions::equations::Equation;
use crate::parser::ast::expressions::expression::{Expression, HandleGuard};
use crate::parser::ast::expressions::guards::GuardVec;

pub(crate) mod funcs;
pub(crate) mod types;

#[derive(Debug, Clone)]
pub(crate) enum DeclarationAst<'a> {
    Value(Expression<'a>, Expression<'a>),
    Function(&'a str, Args<'a>, Expression<'a>),
    FunctionWithGuards(&'a str, Args<'a>, GuardVec<'a>, Option<Vec<Equation<'a>>>),
    TypeDecl(
        &'a str,
        Option<Vec<&'a str>>,
        Vec<AlgebraicType<'a>>,
        Option<Vec<Derivation<'a>>>,
    ),
    TypeAlias(&'a str, Option<Vec<&'a str>>, BaseType<'a>),
    TraitDecl(&'a str, Option<Vec<&'a str>>, Vec<(&'a str, FuncType<'a>)>),
    ExtensionDecl(&'a str, &'a str, Vec<DeclarationAst<'a>>),
    FunctionPrototype(&'a str, FuncType<'a>),
    EffectPrototype(&'a str, FuncType<'a>),
    MacroDecl(Box<DeclarationAst<'a>>),
    Handler(&'a str, Args<'a>, Vec<HandleGuard<'a>>),
    DocString(Option<String>),
}

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

pub(crate) type DeclVec<'a> = Vec<DeclarationAst<'a>>;

pub(crate) type DeclParseResult<'a> = Result<Option<(DeclarationAst<'a>, usize)>>;
