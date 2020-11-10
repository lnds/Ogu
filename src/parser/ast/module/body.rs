use crate::parser::Parser;
use anyhow::Result;

#[derive(Debug)]
pub struct Body {

}

impl<'a> Body {
    pub(crate) fn parse(parser: &Parser<'a>, pos: &usize) -> Result<(Self, usize)> {
        Ok((Body{}, *pos))
    }
}