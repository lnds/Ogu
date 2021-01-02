use thiserror::Error;

#[derive(Error, Debug)]
pub enum OguError {
    #[error("Can't load Figfont")]
    FigfontError,
    #[error("Source not found")]
    NotFound,
    #[error("Parser error")]
    ParserError,
    #[error("Symbol table error")]
    SymbolTableError,
    #[error("Semantic error")]
    SemanticError,
    #[error("code gen error")]
    CodeGenError,
    #[error(transparent)]
    IOError(#[from] std::io::Error),
}
