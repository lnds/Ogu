use thiserror::Error;

#[derive(Error, Debug)]
pub enum OguError {
    #[error("Can't load Figfont")]
    FigfontError(String),
    #[error("Source not found")]
    NotFound(String),
    #[error("Parser error")]
    ParserError(String),
    #[error("Symbol table error")]
    SymbolTableError(String),
    #[error(transparent)]
    IOError(#[from] std::io::Error),
}