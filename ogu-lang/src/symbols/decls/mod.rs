pub(crate) mod funcs;
pub(crate) mod types;
pub(crate) mod values;

use crate::parser::ast::module::decls::Declaration;
use crate::parser::ast::module::decls::Declaration::{Function, Value};
use crate::symbols::decls::funcs::FunctionSym;
use crate::symbols::decls::values::ValueSym;
use crate::symbols::Symbol;

