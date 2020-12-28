pub(crate) mod funcs;
pub(crate) mod types;

use crate::parser::ast::expressions::args::Args;
use crate::parser::ast::expressions::equations::Equation;
use crate::parser::ast::expressions::expression::{Expression, HandleGuard};
use anyhow::Result;

#[derive(Debug, Clone)]
pub(crate) enum Declaration<'a> {
    Value(&'a str, Expression<'a>),
    Function(&'a str, Args<'a>, Expression<'a>),
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

pub(crate) type DeclVec<'a> = Vec<Declaration<'a>>;

pub(crate) type DeclParseResult<'a> = Result<Option<(Declaration<'a>, usize)>>;

impl<'a> Declaration<'a> {
    pub fn get_name(&self) -> &'a str {
        match self {
            Declaration::Value(val, _) => val,
            Declaration::Function(f, _, _) => f,
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
