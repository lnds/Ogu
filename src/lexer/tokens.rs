use crate::lexer::tokens::Symbol::{
    HashCurly, LeftBracket, LeftCurly, LeftParen, RightBracket, RightCurly, RightParen,
};
use logos::{Lexer, Logos};
use std::fmt::Display;

pub type LineSize = usize;

pub type LineNumber = usize;

pub type IndentStack = Vec<LineSize>;

#[derive(Logos, Debug, Clone, Copy, PartialEq)]
pub enum Symbol<'a> {
    Indent,
    Dedent,
    LargeString(usize),
    #[error]
    Error,
    #[regex(r"[ \t]", logos::skip)]
    Ws,
    #[regex(r"--.*", logos::skip)]
    Comment,
    #[regex(r"[\n\r]+")]
    NewLine,
    #[token("alias", priority = 2000)]
    Alias,
    #[token("as", priority = 2000)]
    As,
    #[token("case", priority = 2000)]
    Case,
    #[token("cond", priority = 2000)]
    Cond,
    #[token("deriving", priority = 2000)]
    Deriving,
    #[token("do", priority = 2000)]
    Do,
    #[token("eager", priority = 2000)]
    Eager,
    #[token("elif", priority = 2000)]
    Elif,
    #[token("else", priority = 2000)]
    Else,
    #[token("exposing", priority = 2000)]
    Exposing,
    #[token("extends", priority = 2000)]
    Extends,
    #[token("for", priority = 2000)]
    For,
    #[token("from", priority = 2000)]
    From,
    #[token("if", priority = 2000)]
    If,
    #[token("import", priority = 2000)]
    Import,
    #[token("in", priority = 2000)]
    In,
    #[token("is", priority = 2000)]
    Is,
    #[token("lazy", priority = 2000)]
    Lazy,
    #[token("let", priority = 2000)]
    Let,
    #[token("loop", priority = 2000)]
    Loop,
    #[token("module", priority = 2000)]
    Module,
    #[token("not", priority = 2000)]
    Not,
    #[token("of", priority = 2000)]
    Of,
    #[token("otherwise", priority = 2000)]
    Otherwise,
    #[token("recur", priority = 2000)]
    Recur,
    #[token("reify", priority = 2000)]
    Reify,
    #[token("return", priority = 2000)]
    Return,
    #[token("then", priority = 2000)]
    Then,
    #[token("trait", priority = 2000)]
    Trait,
    #[token("type", priority = 2000)]
    Type,
    #[token("until", priority = 2000)]
    Until,
    #[token("where", priority = 2000)]
    Where,
    #[token("while", priority = 2000)]
    While,
    #[token("with", priority = 2000)]
    With,
    #[token("yield", priority = 2000)]
    Yield,
    #[token("&&", priority = 1000)]
    And,
    #[token("@", priority = 1000)]
    Arroba,
    #[token("->", priority = 1000)]
    Arrow,
    #[token("=", priority = 1000)]
    Assign,
    #[token("<-", priority = 1000)]
    BackArrow,
    #[token(":", priority = 1000)]
    Colon,
    #[token(";", priority = 1000)]
    SemiColon,
    #[token(",", priority = 1000)]
    Comma,
    #[token(">>", priority = 1000)]
    ComposeForward,
    #[token("<<", priority = 1000)]
    ComposeBackward,
    #[token("::", priority = 1000)]
    Cons,
    #[token("/", priority = 1000)]
    Div,
    #[token("//", priority = 1010)]
    DivDiv,
    #[token("$", priority = 1000)]
    Dollar,
    #[token("...", priority = 1000)]
    DotDotDot,
    #[token("..<", priority = 1000)]
    DotDotLess,
    #[token("..", priority = 1000)]
    DotDot,
    #[token(".", priority = 1000)]
    Dot,
    #[token("!>", priority = 1000)]
    Doto,
    #[token("<!", priority = 1000)]
    DotoBack,
    #[token("=>", priority = 1000)]
    FatArrow,
    #[token("==", priority = 1000)]
    Equal,
    #[token(">=", priority = 1000)]
    GreaterOrEqual,
    #[token(">", priority = 1000)]
    Greater,
    #[token("|", priority = 1000)]
    Guard,
    #[token("\\", priority = 1000)]
    Lambda,
    #[token("<=", priority = 1000)]
    LessThanOrEqual,
    #[token("[", priority = 1000)]
    LeftBracket,
    #[token("{", priority = 1000)]
    LeftCurly,
    #[token("#{", priority = 100)]
    HashCurly,
    #[token("(", priority = 1000)]
    LeftParen,
    #[token("<", priority = 1000)]
    LessThan,
    #[token("~", priority = 1000)]
    Match,
    #[token("=~", priority = 1000)]
    Matches,
    #[token("!~", priority = 1000)]
    NotMatches,
    #[token("-", priority = 1000)]
    Minus,
    #[token("%", priority = 1000)]
    Mod,
    #[token("*", priority = 1000)]
    Mult,
    #[token("/=", priority = 1000)]
    NotEqual,
    #[token("||", priority = 1000)]
    Or,
    #[token("<|", priority = 1000)]
    PipeLeft,
    #[token("|<", priority = 1000)]
    PipeLeftFirstArg,
    #[token("|>")]
    PipeRight,
    #[token(">|")]
    PipeRightFirstArg,
    #[token("+", priority = 1000)]
    Plus,
    #[token("++", priority = 1000)]
    PlusPlus,
    #[token("^")]
    Pow,
    #[token("?", priority = 1000)]
    Question,
    #[token("]", priority = 1000)]
    RightBracket,
    #[token("}", priority = 1000)]
    RightCurly,
    #[token(")", priority = 1000)]
    RightParen,
    #[regex(r"[A-Z][_a-zA-Z0-9]*", priority = 110, callback = extract_slice)]
    TypeId(&'a str),
    #[regex(r"[:]?[_a-zA-Z\-\+\*\$<>=][_a-zA-Z0-9\-\+\*\$<>=]*[:!\?']*", priority = 100, callback = extract_slice)]
    Id(&'a str),
    #[regex(r#""([^"]*)""#, priority = 20, callback = extract_string)]
    String(&'a str),
    #[regex(r#"f"([^"]*)""#, priority = 20, callback = extract_f_string)]
    FormatString(&'a str),
    #[regex(r"[\+\-]?[0-9]+[N]?", priority = 2000, callback = extract_slice)]
    Integer(&'a str),
    #[regex(r"[\+\-]?[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?[M]?", priority = 2000, callback = extract_slice)]
    Float(&'a str),
    #[regex(r"[\+\-]?[0-9]+/[0-9]+[M]?", priority = 2000, callback = extract_slice)]
    Ratio(&'a str),
    #[regex(r"#(\d+)-(\d+)-(\d+)(T(\d+):(\d+)(:(\d+)(\.(\d+))?)?(Z|([+-]\d+(:\d+)?))?)?", callback = extract_slice)]
    IsoDate(&'a str),
    #[regex(r"#((/[^/]*/)|(`[^`]*`))#", callback = extract_regex)]
    RegExp(&'a str),
    #[regex(r#"'(.|\\n|\\r|\\t|\\u[0-9]+)'"#, callback = extract_string)]
    Char(&'a str),
}

impl<'a> Symbol<'a> {
    pub fn is_open_paren(&self) -> bool {
        *self == LeftParen || *self == LeftBracket || *self == LeftCurly || *self == HashCurly
    }

    pub fn is_close_paren(&self) -> bool {
        *self == RightParen || *self == RightBracket || *self == RightCurly
    }
}

impl<'a> Display for Symbol<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{:?}", self)
    }
}

fn extract_string<'a>(lex: &mut Lexer<'a, Symbol<'a>>) -> Option<&'a str> {
    let slice = lex.slice();
    Some(&slice[1..slice.len() - 1])
}

fn extract_f_string<'a>(lex: &mut Lexer<'a, Symbol<'a>>) -> Option<&'a str> {
    let slice = lex.slice();
    Some(&slice[2..slice.len() - 1])
}

fn extract_slice<'a>(lex: &mut Lexer<'a, Symbol<'a>>) -> Option<&'a str> {
    let slice = lex.slice();
    Some(slice)
}

fn extract_regex<'a>(lex: &mut Lexer<'a, Symbol<'a>>) -> Option<&'a str> {
    let slice = lex.slice();
    Some(&slice[2..slice.len() - 2])
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub symbol: Symbol<'a>,
    pub line: LineSize,
}

impl<'a> Token<'a> {
    pub fn new(symbol: Symbol<'a>, line: LineSize) -> Self {
        Token { symbol, line }
    }
}

pub type TokenList<'a> = Vec<Token<'a>>;

pub type SymbolList<'a> = Vec<Symbol<'a>>;

#[cfg(test)]
mod test_tokens {
    use crate::lexer::tokens::Symbol;
    use logos::Logos;

    #[test]
    fn test_symbols() {
        let mut lex = Symbol::lexer("(a+b)");
        assert_eq!(lex.next(), Some(Symbol::LeftParen));
        assert_eq!(lex.next(), Some(Symbol::Id("a+b")));
        assert_eq!(lex.slice(), "a+b");
        assert_eq!(lex.next(), Some(Symbol::RightParen));
    }

    #[test]
    fn test_opers() {
        let mut lex = Symbol::lexer("(a + b)");
        assert_eq!(lex.next(), Some(Symbol::LeftParen));
        assert_eq!(lex.next(), Some(Symbol::Id("a")));
        assert_eq!(lex.slice(), "a");
        assert_eq!(lex.next(), Some(Symbol::Plus));
        assert_eq!(lex.next(), Some(Symbol::Id("b")));
        assert_eq!(lex.slice(), "b");
        assert_eq!(lex.next(), Some(Symbol::RightParen));
    }

    #[test]
    fn test_string() {
        let mut lex = Symbol::lexer("\"ab\" ++ \"cd\"");
        assert_eq!(lex.next(), Some(Symbol::String("ab")));
        assert_eq!(lex.next(), Some(Symbol::PlusPlus));
        assert_eq!(lex.next(), Some(Symbol::String("cd")));
    }

    #[test]
    fn test_comments() {
        let mut lex = Symbol::lexer("ab ++ cd -- with comments");
        assert_eq!(lex.next(), Some(Symbol::Id("ab")));
        assert_eq!(lex.next(), Some(Symbol::PlusPlus));
        assert_eq!(lex.next(), Some(Symbol::Id("cd")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_ids() {
        let mut lex = Symbol::lexer("a.b");
        assert_eq!(lex.next(), Some(Symbol::Id("a")));
        assert_eq!(lex.next(), Some(Symbol::Dot));
        assert_eq!(lex.next(), Some(Symbol::Id("b")));
        assert_eq!(lex.next(), None);
        let mut lex = Symbol::lexer("Type.b");
        assert_eq!(lex.next(), Some(Symbol::TypeId("Type")));
        assert_eq!(lex.next(), Some(Symbol::Dot));
        assert_eq!(lex.next(), Some(Symbol::Id("b")));
        assert_eq!(lex.next(), None);
        let mut lex = Symbol::lexer("Type.a+b");
        assert_eq!(lex.next(), Some(Symbol::TypeId("Type")));
        assert_eq!(lex.next(), Some(Symbol::Dot));
        assert_eq!(lex.next(), Some(Symbol::Id("a+b")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("Type.**weird-id**");
        assert_eq!(lex.next(), Some(Symbol::TypeId("Type")));
        assert_eq!(lex.next(), Some(Symbol::Dot));
        assert_eq!(lex.next(), Some(Symbol::Id("**weird-id**")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("Type.**weird-id**.<name>.$value.bang!.question?");
        assert_eq!(lex.next(), Some(Symbol::TypeId("Type")));
        assert_eq!(lex.next(), Some(Symbol::Dot));
        assert_eq!(lex.next(), Some(Symbol::Id("**weird-id**")));
        assert_eq!(lex.next(), Some(Symbol::Dot));
        assert_eq!(lex.next(), Some(Symbol::Id("<name>")));
        assert_eq!(lex.next(), Some(Symbol::Dot));
        assert_eq!(lex.next(), Some(Symbol::Id("$value")));
        assert_eq!(lex.next(), Some(Symbol::Dot));
        assert_eq!(lex.next(), Some(Symbol::Id("bang!")));
        assert_eq!(lex.next(), Some(Symbol::Dot));
        assert_eq!(lex.next(), Some(Symbol::Id("question?")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer(":id :lost+found");
        assert_eq!(lex.next(), Some(Symbol::Id(":id")));
        assert_eq!(lex.next(), Some(Symbol::Id(":lost+found")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_keywords() {
        let mut lex = Symbol::lexer("as case cond do deriving eager elif else");
        assert_eq!(lex.next(), Some(Symbol::As));
        assert_eq!(lex.next(), Some(Symbol::Case));
        assert_eq!(lex.next(), Some(Symbol::Cond));
        assert_eq!(lex.next(), Some(Symbol::Do));
        assert_eq!(lex.next(), Some(Symbol::Deriving));
        assert_eq!(lex.next(), Some(Symbol::Eager));
        assert_eq!(lex.next(), Some(Symbol::Elif));
        assert_eq!(lex.next(), Some(Symbol::Else));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("exposing extends for from if in is lazy");
        assert_eq!(lex.next(), Some(Symbol::Exposing));
        assert_eq!(lex.next(), Some(Symbol::Extends));
        assert_eq!(lex.next(), Some(Symbol::For));
        assert_eq!(lex.next(), Some(Symbol::From));
        assert_eq!(lex.next(), Some(Symbol::If));
        assert_eq!(lex.next(), Some(Symbol::In));
        assert_eq!(lex.next(), Some(Symbol::Is));
        assert_eq!(lex.next(), Some(Symbol::Lazy));
        assert_eq!(lex.next(), None);

        let mut lex =
            Symbol::lexer("let loop module not of otherwise recur reify return then trait until");
        assert_eq!(lex.next(), Some(Symbol::Let));
        assert_eq!(lex.next(), Some(Symbol::Loop));
        assert_eq!(lex.next(), Some(Symbol::Module));
        assert_eq!(lex.next(), Some(Symbol::Not));
        assert_eq!(lex.next(), Some(Symbol::Of));
        assert_eq!(lex.next(), Some(Symbol::Otherwise));
        assert_eq!(lex.next(), Some(Symbol::Recur));
        assert_eq!(lex.next(), Some(Symbol::Reify));
        assert_eq!(lex.next(), Some(Symbol::Return));
        assert_eq!(lex.next(), Some(Symbol::Then));
        assert_eq!(lex.next(), Some(Symbol::Trait));
        assert_eq!(lex.next(), Some(Symbol::Until));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("type where with yield");
        assert_eq!(lex.next(), Some(Symbol::Type));
        assert_eq!(lex.next(), Some(Symbol::Where));
        assert_eq!(lex.next(), Some(Symbol::With));
        assert_eq!(lex.next(), Some(Symbol::Yield));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_ops() {
        let mut lex = Symbol::lexer("&&  @ -> = <- : , >>");
        assert_eq!(lex.next(), Some(Symbol::And));
        assert_eq!(lex.next(), Some(Symbol::Arroba));
        assert_eq!(lex.next(), Some(Symbol::Arrow));
        assert_eq!(lex.next(), Some(Symbol::Assign));
        assert_eq!(lex.next(), Some(Symbol::BackArrow));
        assert_eq!(lex.next(), Some(Symbol::Colon));
        assert_eq!(lex.next(), Some(Symbol::Comma));
        assert_eq!(lex.next(), Some(Symbol::ComposeForward));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("<< :: / // $ ... ..< .. . !>");
        assert_eq!(lex.next(), Some(Symbol::ComposeBackward));
        assert_eq!(lex.next(), Some(Symbol::Cons));
        assert_eq!(lex.next(), Some(Symbol::Div));
        assert_eq!(lex.next(), Some(Symbol::DivDiv));
        assert_eq!(lex.next(), Some(Symbol::Dollar));
        assert_eq!(lex.next(), Some(Symbol::DotDotDot));
        assert_eq!(lex.next(), Some(Symbol::DotDotLess));
        assert_eq!(lex.next(), Some(Symbol::DotDot));
        assert_eq!(lex.next(), Some(Symbol::Dot));
        assert_eq!(lex.next(), Some(Symbol::Doto));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("<! == >= > | \\ <= [ { #{ (");
        assert_eq!(lex.next(), Some(Symbol::DotoBack));
        assert_eq!(lex.next(), Some(Symbol::Equal));
        assert_eq!(lex.next(), Some(Symbol::GreaterOrEqual));
        assert_eq!(lex.next(), Some(Symbol::Greater));
        assert_eq!(lex.next(), Some(Symbol::Guard));
        assert_eq!(lex.next(), Some(Symbol::Lambda));
        assert_eq!(lex.next(), Some(Symbol::LessThanOrEqual));
        assert_eq!(lex.next(), Some(Symbol::LeftBracket));
        assert_eq!(lex.next(), Some(Symbol::LeftCurly));
        assert_eq!(lex.next(), Some(Symbol::HashCurly));
        assert_eq!(lex.next(), Some(Symbol::LeftParen));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("< ~ =~ - % * /= || <| |<");
        assert_eq!(lex.next(), Some(Symbol::LessThan));
        assert_eq!(lex.next(), Some(Symbol::Match));
        assert_eq!(lex.next(), Some(Symbol::Matches));
        assert_eq!(lex.next(), Some(Symbol::Minus));
        assert_eq!(lex.next(), Some(Symbol::Mod));
        assert_eq!(lex.next(), Some(Symbol::Mult));
        assert_eq!(lex.next(), Some(Symbol::NotEqual));
        assert_eq!(lex.next(), Some(Symbol::Or));
        assert_eq!(lex.next(), Some(Symbol::PipeLeft));
        assert_eq!(lex.next(), Some(Symbol::PipeLeftFirstArg));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("|> >| + ++ ^ ? ] } )");
        assert_eq!(lex.next(), Some(Symbol::PipeRight));
        assert_eq!(lex.next(), Some(Symbol::PipeRightFirstArg));
        assert_eq!(lex.next(), Some(Symbol::Plus));
        assert_eq!(lex.next(), Some(Symbol::PlusPlus));
        assert_eq!(lex.next(), Some(Symbol::Pow));
        assert_eq!(lex.next(), Some(Symbol::Question));
        assert_eq!(lex.next(), Some(Symbol::RightBracket));
        assert_eq!(lex.next(), Some(Symbol::RightCurly));
        assert_eq!(lex.next(), Some(Symbol::RightParen));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_literals() {
        let mut lex = Symbol::lexer("2 + 2");
        assert_eq!(lex.next(), Some(Symbol::Integer("2")));
        assert_eq!(lex.next(), Some(Symbol::Plus));
        assert_eq!(lex.next(), Some(Symbol::Integer("2")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("1234567890 123.4567890");
        assert_eq!(lex.next(), Some(Symbol::Integer("1234567890")));
        assert_eq!(lex.next(), Some(Symbol::Float("123.4567890")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("12/34 1/3 2.45E10 3.4e-20");
        assert_eq!(lex.next(), Some(Symbol::Ratio("12/34")));
        assert_eq!(lex.next(), Some(Symbol::Ratio("1/3")));
        assert_eq!(lex.next(), Some(Symbol::Float("2.45E10")));
        assert_eq!(lex.next(), Some(Symbol::Float("3.4e-20")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("-1/4 +1/4 -0.4 +0.4 +32 -32 .333 -.455");
        assert_eq!(lex.next(), Some(Symbol::Ratio("-1/4")));
        assert_eq!(lex.next(), Some(Symbol::Ratio("+1/4")));
        assert_eq!(lex.next(), Some(Symbol::Float("-0.4")));
        assert_eq!(lex.next(), Some(Symbol::Float("+0.4")));
        assert_eq!(lex.next(), Some(Symbol::Integer("+32")));
        assert_eq!(lex.next(), Some(Symbol::Integer("-32")));
        assert_eq!(lex.next(), Some(Symbol::Float(".333")));
        assert_eq!(lex.next(), Some(Symbol::Float("-.455")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("2.4M 200N");
        assert_eq!(lex.next(), Some(Symbol::Float("2.4M")));
        assert_eq!(lex.next(), Some(Symbol::Integer("200N")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_ranges() {
        let mut lex = Symbol::lexer("a..b");
        assert_eq!(lex.next(), Some(Symbol::Id("a")));
        assert_eq!(lex.next(), Some(Symbol::DotDot));
        assert_eq!(lex.next(), Some(Symbol::Id("b")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("1..2");
        assert_eq!(lex.next(), Some(Symbol::Integer("1")));
        assert_eq!(lex.next(), Some(Symbol::DotDot));
        assert_eq!(lex.next(), Some(Symbol::Integer("2")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("1..<2");
        assert_eq!(lex.next(), Some(Symbol::Integer("1")));
        assert_eq!(lex.next(), Some(Symbol::DotDotLess));
        assert_eq!(lex.next(), Some(Symbol::Integer("2")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("1...");
        assert_eq!(lex.next(), Some(Symbol::Integer("1")));
        assert_eq!(lex.next(), Some(Symbol::DotDotDot));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("1.4..3.4");
        assert_eq!(lex.next(), Some(Symbol::Float("1.4")));
        assert_eq!(lex.next(), Some(Symbol::DotDot));
        assert_eq!(lex.next(), Some(Symbol::Float("3.4")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_indent() {
        let mut lex = Symbol::lexer("  a");
        assert_eq!(lex.next(), Some(Symbol::Id("a")));
    }

    #[test]
    fn test_some_expressions() {
        let mut lex = Symbol::lexer("\"a b c d\" |> .toUpperCase |> .split \" \" |> first");
        assert_eq!(lex.next(), Some(Symbol::String("a b c d")));
        assert_eq!(lex.next(), Some(Symbol::PipeRight));
        assert_eq!(lex.next(), Some(Symbol::Dot));
        assert_eq!(lex.next(), Some(Symbol::Id("toUpperCase")));
        assert_eq!(lex.next(), Some(Symbol::PipeRight));
        assert_eq!(lex.next(), Some(Symbol::Dot));
        assert_eq!(lex.next(), Some(Symbol::Id("split")));
        assert_eq!(lex.next(), Some(Symbol::String(" ")));
        assert_eq!(lex.next(), Some(Symbol::PipeRight));
        assert_eq!(lex.next(), Some(Symbol::Id("first")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_dates() {
        let mut lex = Symbol::lexer("#2017-02-26");
        assert_eq!(lex.next(), Some(Symbol::IsoDate("#2017-02-26")));
        let mut lex = Symbol::lexer("#2017-02-26T23:50");
        assert_eq!(lex.next(), Some(Symbol::IsoDate("#2017-02-26T23:50")));
        let mut lex = Symbol::lexer("#2017-02-26T23:50:30");
        assert_eq!(lex.next(), Some(Symbol::IsoDate("#2017-02-26T23:50:30")));
        let mut lex = Symbol::lexer("#2017-02-26T23:50:30.120");
        assert_eq!(
            lex.next(),
            Some(Symbol::IsoDate("#2017-02-26T23:50:30.120"))
        );
        let mut lex = Symbol::lexer("#2017-02-26T23:50:30.120Z");
        assert_eq!(
            lex.next(),
            Some(Symbol::IsoDate("#2017-02-26T23:50:30.120Z"))
        );
        let mut lex = Symbol::lexer("#2017-02-26T23:50:30.120-3");
        assert_eq!(
            lex.next(),
            Some(Symbol::IsoDate("#2017-02-26T23:50:30.120-3"))
        );
        let mut lex = Symbol::lexer("#2017-02-26T23:50:30.120+3");
        assert_eq!(
            lex.next(),
            Some(Symbol::IsoDate("#2017-02-26T23:50:30.120+3"))
        );
        let mut lex = Symbol::lexer("#2017-02-26T23:50:30.120+3:30");
        assert_eq!(
            lex.next(),
            Some(Symbol::IsoDate("#2017-02-26T23:50:30.120+3:30"))
        );
    }

    #[test]
    fn test_regex() {
        let mut lex = Symbol::lexer("\"aaabbb\" =~ #/(a|b)+/#");
        assert_eq!(lex.next(), Some(Symbol::String("aaabbb")));
        assert_eq!(lex.next(), Some(Symbol::Matches));
        assert_eq!(lex.next(), Some(Symbol::RegExp("(a|b)+")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("\"aaabbb\" =~ #`(a|b|/)+`#");
        assert_eq!(lex.next(), Some(Symbol::String("aaabbb")));
        assert_eq!(lex.next(), Some(Symbol::Matches));

        assert_eq!(lex.next(), Some(Symbol::RegExp("(a|b|/)+")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_strings() {
        let mut lex = Symbol::lexer("msj = f\"\nIngrese su apuesta (min: ${apuesta-minima}, max: ${pozo}, 0 para finalizar juego): \"");
        assert_eq!(lex.next(), Some(Symbol::Id("msj")));
        assert_eq!(lex.next(), Some(Symbol::Assign));
        assert_eq!(lex.next(), Some(Symbol::FormatString("\nIngrese su apuesta (min: ${apuesta-minima}, max: ${pozo}, 0 para finalizar juego): ")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_chars() {
        let mut lex = Symbol::lexer("'a' 'b' '\\n' '\\u3829'");
        assert_eq!(lex.next(), Some(Symbol::Char("a")));
        assert_eq!(lex.next(), Some(Symbol::Char("b")));
        assert_eq!(lex.next(), Some(Symbol::Char("\\n")));
        assert_eq!(lex.next(), Some(Symbol::Char("\\u3829")));
        assert_eq!(lex.next(), None);
    }
}
