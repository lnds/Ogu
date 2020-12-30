use crate::parser::ast::expressions::expression::Expression;
use crate::backend::modules::symbols::exprs::literals::LiteralSym;
use crate::backend::scopes::symbol::Symbol;

mod literals;


impl<'a> From<&Expression<'a>> for Box<dyn Symbol> {

    fn from(expr: &Expression<'a>) -> Self {
        match expr {
            Expression::IntegerLiteral(l) => LiteralSym::new_int(l),
            Expression::FloatLiteral(f) => LiteralSym::new_float(f),
            Expression::StringLiteral(s) => LiteralSym::new_str(s),
            _ => todo!()
        }
    }
}