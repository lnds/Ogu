use logos::{Lexer, Logos};
use std::fmt::Display;

pub(crate) type LineCount = usize;

pub(crate) type LineWidth = usize;
pub(crate) type LineNumber = usize;

pub(crate) type IndentStack = Vec<LineCount>;

#[derive(Logos, Debug, Clone, Copy, PartialEq)]
pub(crate) enum Token<'a> {
    Indent,
    Dedent,
    LargeString(usize),
    #[error]
    Error,
    #[regex(r"[ \t]", logos::skip)]
    Ws,
    #[regex(r"--.*", logos::skip, priority = 100000)]
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
    #[token("derive", priority = 2000)]
    Derive,
    #[token("do", priority = 2000)]
    Do,
    #[token("eager", priority = 2000)]
    Eager,
    #[token("effect", priority = 2000)]
    Effect,
    #[token("elif", priority = 2000)]
    Elif,
    #[token("else", priority = 2000)]
    Else,
    #[token("exposing", priority = 2000)]
    Exposing,
    #[token("extends", priority = 2000)]
    Extends,
    #[token("extern", priority = 2000)]
    Extern,
    #[token("for", priority = 2000)]
    For,
    #[token("from", priority = 2000)]
    From,
    #[token("handle", priority = 2000)]
    Handle,
    #[token("handler", priority = 2000)]
    Handler,
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
    #[token("macro", priority = 2000)]
    Macro,
    #[token("module", priority = 2000)]
    Module,
    #[token("not", priority = 2000)]
    Not,
    #[token("of", priority = 2000)]
    Of,
    #[token("otherwise", priority = 2000)]
    Otherwise,
    #[token("perform", priority = 2000)]
    Perform,
    #[token("primitive", priority = 2000)]
    Primitive,
    #[token("recur", priority = 2000)]
    Recur,
    #[token("repeat", priority = 2000)]
    Repeat,
    #[token("reify", priority = 2000)]
    Reify,
    #[token("return", priority = 2000)]
    Return,
    #[token("resume", priority = 2000)]
    Resume,
    #[token("then", priority = 2000)]
    Then,
    #[token("trait", priority = 2000)]
    Trait,
    #[token("try", priority = 2000)]
    Try,
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
    #[token("${", priority = 1000)]
    DollarCurly,
    #[token("...", priority = 1000)]
    DotDotDot,
    #[token("..<", priority = 1000)]
    DotDotLess,
    #[token("..", priority = 1000)]
    DotDot,
    #[token(".", priority = 1000)]
    Dot,
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
    #[token("{{", priority = 1000)]
    LeftCurlyCurly,
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
    #[token("|>")]
    PipeRight,
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
    #[token("}}", priority = 1000)]
    RightCurlyCurly,
    #[token("}", priority = 1000)]
    RightCurly,
    #[token(")", priority = 1000)]
    RightParen,
    #[regex(r"[A-Z][_a-zA-Z0-9]*", priority = 110, callback = extract_slice)]
    TypeId(&'a str),
    #[regex(r"[_a-zA-Z\-\+\*\$][_a-zA-Z0-9\-\+\*\$]*[!\?']*", priority = 100, callback = extract_slice)]
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

impl<'a> Token<'a> {
    pub(crate) fn is_open_paren(&self) -> bool {
        matches!(
            *self,
            Token::LeftParen
                | Token::LeftBracket
                | Token::LeftCurly
                | Token::LeftCurlyCurly
                | Token::HashCurly
                | Token::DollarCurly
        )
    }

    pub(crate) fn is_close_paren(&self) -> bool {
        matches!(
            *self,
            Token::RightParen | Token::RightBracket | Token::RightCurly | Token::RightCurlyCurly
        )
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{:?}", self)
    }
}

fn extract_string<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<&'a str> {
    let slice = lex.slice();
    Some(&slice[1..slice.len() - 1])
}

fn extract_f_string<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<&'a str> {
    let slice = lex.slice();
    Some(&slice[2..slice.len() - 1])
}

fn extract_slice<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<&'a str> {
    let slice = lex.slice();
    Some(slice)
}

fn extract_regex<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<&'a str> {
    let slice = lex.slice();
    Some(&slice[2..slice.len() - 2])
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TokenContext<'a> {
    pub(crate) token: Token<'a>,
    pub(crate) line: LineCount,
    pub(crate) col: LineWidth,
}

impl<'a> TokenContext<'a> {
    pub(crate) fn new(symbol: Token<'a>, line: LineCount, col: LineWidth) -> Self {
        TokenContext {
            token: symbol,
            line,
            col,
        }
    }
}

pub(crate) type TokenContextList<'a> = Vec<TokenContext<'a>>;

#[cfg(test)]
mod test_tokens {
    use crate::lexer::tokens::Token;
    use logos::Logos;

    #[test]
    fn test_symbols() {
        let mut lex = Token::lexer("(a+b)");
        assert_eq!(lex.next(), Some(Token::LeftParen));
        assert_eq!(lex.next(), Some(Token::Id("a+b")));
        assert_eq!(lex.slice(), "a+b");
        assert_eq!(lex.next(), Some(Token::RightParen));
    }

    #[test]
    fn test_opers() {
        let mut lex = Token::lexer("(a + b)");
        assert_eq!(lex.next(), Some(Token::LeftParen));
        assert_eq!(lex.next(), Some(Token::Id("a")));
        assert_eq!(lex.slice(), "a");
        assert_eq!(lex.next(), Some(Token::Plus));
        assert_eq!(lex.next(), Some(Token::Id("b")));
        assert_eq!(lex.slice(), "b");
        assert_eq!(lex.next(), Some(Token::RightParen));
    }

    #[test]
    fn test_string() {
        let mut lex = Token::lexer("\"ab\" ++ \"cd\"");
        assert_eq!(lex.next(), Some(Token::String("ab")));
        assert_eq!(lex.next(), Some(Token::PlusPlus));
        assert_eq!(lex.next(), Some(Token::String("cd")));
    }

    #[test]
    fn test_comments() {
        let mut lex = Token::lexer("ab ++ cd -- with comments");
        assert_eq!(lex.next(), Some(Token::Id("ab")));
        assert_eq!(lex.next(), Some(Token::PlusPlus));
        assert_eq!(lex.next(), Some(Token::Id("cd")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_ids() {
        let mut lex = Token::lexer("a.b");
        assert_eq!(lex.next(), Some(Token::Id("a")));
        assert_eq!(lex.next(), Some(Token::Dot));
        assert_eq!(lex.next(), Some(Token::Id("b")));
        assert_eq!(lex.next(), None);
        let mut lex = Token::lexer("Type.b");
        assert_eq!(lex.next(), Some(Token::TypeId("Type")));
        assert_eq!(lex.next(), Some(Token::Dot));
        assert_eq!(lex.next(), Some(Token::Id("b")));
        assert_eq!(lex.next(), None);
        let mut lex = Token::lexer("Type.a+b");
        assert_eq!(lex.next(), Some(Token::TypeId("Type")));
        assert_eq!(lex.next(), Some(Token::Dot));
        assert_eq!(lex.next(), Some(Token::Id("a+b")));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("Type.**weird-id**");
        assert_eq!(lex.next(), Some(Token::TypeId("Type")));
        assert_eq!(lex.next(), Some(Token::Dot));
        assert_eq!(lex.next(), Some(Token::Id("**weird-id**")));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("Type.**weird-id**.name.$value.bang!.question?");
        assert_eq!(lex.next(), Some(Token::TypeId("Type")));
        assert_eq!(lex.next(), Some(Token::Dot));
        assert_eq!(lex.next(), Some(Token::Id("**weird-id**")));
        assert_eq!(lex.next(), Some(Token::Dot));
        assert_eq!(lex.next(), Some(Token::Id("name")));
        assert_eq!(lex.next(), Some(Token::Dot));
        assert_eq!(lex.next(), Some(Token::Id("$value")));
        assert_eq!(lex.next(), Some(Token::Dot));
        assert_eq!(lex.next(), Some(Token::Id("bang!")));
        assert_eq!(lex.next(), Some(Token::Dot));
        assert_eq!(lex.next(), Some(Token::Id("question?")));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("id lost+found");
        assert_eq!(lex.next(), Some(Token::Id("id")));
        assert_eq!(lex.next(), Some(Token::Id("lost+found")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_keywords() {
        let mut lex = Token::lexer("as case cond do derive eager effect elif else");
        assert_eq!(lex.next(), Some(Token::As));
        assert_eq!(lex.next(), Some(Token::Case));
        assert_eq!(lex.next(), Some(Token::Cond));
        assert_eq!(lex.next(), Some(Token::Do));
        assert_eq!(lex.next(), Some(Token::Derive));
        assert_eq!(lex.next(), Some(Token::Eager));
        assert_eq!(lex.next(), Some(Token::Effect));
        assert_eq!(lex.next(), Some(Token::Elif));
        assert_eq!(lex.next(), Some(Token::Else));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("exposing extends extern for from if handle handler in is lazy");
        assert_eq!(lex.next(), Some(Token::Exposing));
        assert_eq!(lex.next(), Some(Token::Extends));
        assert_eq!(lex.next(), Some(Token::Extern));
        assert_eq!(lex.next(), Some(Token::For));
        assert_eq!(lex.next(), Some(Token::From));
        assert_eq!(lex.next(), Some(Token::If));
        assert_eq!(lex.next(), Some(Token::Handle));
        assert_eq!(lex.next(), Some(Token::Handler));
        assert_eq!(lex.next(), Some(Token::In));
        assert_eq!(lex.next(), Some(Token::Is));
        assert_eq!(lex.next(), Some(Token::Lazy));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer(
            "let loop macro module not of otherwise perform primitive repeat recur reify resume return then trait try until",
        );
        assert_eq!(lex.next(), Some(Token::Let));
        assert_eq!(lex.next(), Some(Token::Loop));
        assert_eq!(lex.next(), Some(Token::Macro));
        assert_eq!(lex.next(), Some(Token::Module));
        assert_eq!(lex.next(), Some(Token::Not));
        assert_eq!(lex.next(), Some(Token::Of));
        assert_eq!(lex.next(), Some(Token::Otherwise));
        assert_eq!(lex.next(), Some(Token::Perform));
        assert_eq!(lex.next(), Some(Token::Primitive));
        assert_eq!(lex.next(), Some(Token::Repeat));
        assert_eq!(lex.next(), Some(Token::Recur));
        assert_eq!(lex.next(), Some(Token::Reify));
        assert_eq!(lex.next(), Some(Token::Resume));
        assert_eq!(lex.next(), Some(Token::Return));
        assert_eq!(lex.next(), Some(Token::Then));
        assert_eq!(lex.next(), Some(Token::Trait));
        assert_eq!(lex.next(), Some(Token::Try));
        assert_eq!(lex.next(), Some(Token::Until));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("type where with yield");
        assert_eq!(lex.next(), Some(Token::Type));
        assert_eq!(lex.next(), Some(Token::Where));
        assert_eq!(lex.next(), Some(Token::With));
        assert_eq!(lex.next(), Some(Token::Yield));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_ops() {
        let mut lex = Token::lexer("&&  @ -> = <- : , >>");
        assert_eq!(lex.next(), Some(Token::And));
        assert_eq!(lex.next(), Some(Token::Arroba));
        assert_eq!(lex.next(), Some(Token::Arrow));
        assert_eq!(lex.next(), Some(Token::Assign));
        assert_eq!(lex.next(), Some(Token::BackArrow));
        assert_eq!(lex.next(), Some(Token::Colon));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::ComposeForward));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("<< :: / // $ ... ..< .. . ");
        assert_eq!(lex.next(), Some(Token::ComposeBackward));
        assert_eq!(lex.next(), Some(Token::Cons));
        assert_eq!(lex.next(), Some(Token::Div));
        assert_eq!(lex.next(), Some(Token::DivDiv));
        assert_eq!(lex.next(), Some(Token::Dollar));
        assert_eq!(lex.next(), Some(Token::DotDotDot));
        assert_eq!(lex.next(), Some(Token::DotDotLess));
        assert_eq!(lex.next(), Some(Token::DotDot));
        assert_eq!(lex.next(), Some(Token::Dot));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("== >= > | \\ <= [ { {{ #{ (");
        assert_eq!(lex.next(), Some(Token::Equal));
        assert_eq!(lex.next(), Some(Token::GreaterOrEqual));
        assert_eq!(lex.next(), Some(Token::Greater));
        assert_eq!(lex.next(), Some(Token::Guard));
        assert_eq!(lex.next(), Some(Token::Lambda));
        assert_eq!(lex.next(), Some(Token::LessThanOrEqual));
        assert_eq!(lex.next(), Some(Token::LeftBracket));
        assert_eq!(lex.next(), Some(Token::LeftCurly));
        assert_eq!(lex.next(), Some(Token::LeftCurlyCurly));
        assert_eq!(lex.next(), Some(Token::HashCurly));
        assert_eq!(lex.next(), Some(Token::LeftParen));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("< ~ =~ - % * /= || <|");
        assert_eq!(lex.next(), Some(Token::LessThan));
        assert_eq!(lex.next(), Some(Token::Match));
        assert_eq!(lex.next(), Some(Token::Matches));
        assert_eq!(lex.next(), Some(Token::Minus));
        assert_eq!(lex.next(), Some(Token::Mod));
        assert_eq!(lex.next(), Some(Token::Mult));
        assert_eq!(lex.next(), Some(Token::NotEqual));
        assert_eq!(lex.next(), Some(Token::Or));
        assert_eq!(lex.next(), Some(Token::PipeLeft));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("|> + ++ ^ ? ] } }} )");
        assert_eq!(lex.next(), Some(Token::PipeRight));
        assert_eq!(lex.next(), Some(Token::Plus));
        assert_eq!(lex.next(), Some(Token::PlusPlus));
        assert_eq!(lex.next(), Some(Token::Pow));
        assert_eq!(lex.next(), Some(Token::Question));
        assert_eq!(lex.next(), Some(Token::RightBracket));
        assert_eq!(lex.next(), Some(Token::RightCurly));
        assert_eq!(lex.next(), Some(Token::RightCurlyCurly));
        assert_eq!(lex.next(), Some(Token::RightParen));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_literals() {
        let mut lex = Token::lexer("2 + 2");
        assert_eq!(lex.next(), Some(Token::Integer("2")));
        assert_eq!(lex.next(), Some(Token::Plus));
        assert_eq!(lex.next(), Some(Token::Integer("2")));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("1234567890 123.4567890");
        assert_eq!(lex.next(), Some(Token::Integer("1234567890")));
        assert_eq!(lex.next(), Some(Token::Float("123.4567890")));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("12/34 1/3 2.45E10 3.4e-20");
        assert_eq!(lex.next(), Some(Token::Ratio("12/34")));
        assert_eq!(lex.next(), Some(Token::Ratio("1/3")));
        assert_eq!(lex.next(), Some(Token::Float("2.45E10")));
        assert_eq!(lex.next(), Some(Token::Float("3.4e-20")));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("-1/4 +1/4 -0.4 +0.4 +32 -32 .333 -.455");
        assert_eq!(lex.next(), Some(Token::Ratio("-1/4")));
        assert_eq!(lex.next(), Some(Token::Ratio("+1/4")));
        assert_eq!(lex.next(), Some(Token::Float("-0.4")));
        assert_eq!(lex.next(), Some(Token::Float("+0.4")));
        assert_eq!(lex.next(), Some(Token::Integer("+32")));
        assert_eq!(lex.next(), Some(Token::Integer("-32")));
        assert_eq!(lex.next(), Some(Token::Float(".333")));
        assert_eq!(lex.next(), Some(Token::Float("-.455")));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("2.4M 200N");
        assert_eq!(lex.next(), Some(Token::Float("2.4M")));
        assert_eq!(lex.next(), Some(Token::Integer("200N")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_ranges() {
        let mut lex = Token::lexer("a..b");
        assert_eq!(lex.next(), Some(Token::Id("a")));
        assert_eq!(lex.next(), Some(Token::DotDot));
        assert_eq!(lex.next(), Some(Token::Id("b")));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("1..2");
        assert_eq!(lex.next(), Some(Token::Integer("1")));
        assert_eq!(lex.next(), Some(Token::DotDot));
        assert_eq!(lex.next(), Some(Token::Integer("2")));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("1..<2");
        assert_eq!(lex.next(), Some(Token::Integer("1")));
        assert_eq!(lex.next(), Some(Token::DotDotLess));
        assert_eq!(lex.next(), Some(Token::Integer("2")));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("1...");
        assert_eq!(lex.next(), Some(Token::Integer("1")));
        assert_eq!(lex.next(), Some(Token::DotDotDot));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("1.4..3.4");
        assert_eq!(lex.next(), Some(Token::Float("1.4")));
        assert_eq!(lex.next(), Some(Token::DotDot));
        assert_eq!(lex.next(), Some(Token::Float("3.4")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_indent() {
        let mut lex = Token::lexer("  a");
        assert_eq!(lex.next(), Some(Token::Id("a")));
    }

    #[test]
    fn test_some_expressions() {
        let mut lex = Token::lexer("\"a b c d\" |> .toUpperCase |> .split \" \" |> first");
        assert_eq!(lex.next(), Some(Token::String("a b c d")));
        assert_eq!(lex.next(), Some(Token::PipeRight));
        assert_eq!(lex.next(), Some(Token::Dot));
        assert_eq!(lex.next(), Some(Token::Id("toUpperCase")));
        assert_eq!(lex.next(), Some(Token::PipeRight));
        assert_eq!(lex.next(), Some(Token::Dot));
        assert_eq!(lex.next(), Some(Token::Id("split")));
        assert_eq!(lex.next(), Some(Token::String(" ")));
        assert_eq!(lex.next(), Some(Token::PipeRight));
        assert_eq!(lex.next(), Some(Token::Id("first")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_dates() {
        let mut lex = Token::lexer("#2017-02-26");
        assert_eq!(lex.next(), Some(Token::IsoDate("#2017-02-26")));
        let mut lex = Token::lexer("#2017-02-26T23:50");
        assert_eq!(lex.next(), Some(Token::IsoDate("#2017-02-26T23:50")));
        let mut lex = Token::lexer("#2017-02-26T23:50:30");
        assert_eq!(lex.next(), Some(Token::IsoDate("#2017-02-26T23:50:30")));
        let mut lex = Token::lexer("#2017-02-26T23:50:30.120");
        assert_eq!(lex.next(), Some(Token::IsoDate("#2017-02-26T23:50:30.120")));
        let mut lex = Token::lexer("#2017-02-26T23:50:30.120Z");
        assert_eq!(
            lex.next(),
            Some(Token::IsoDate("#2017-02-26T23:50:30.120Z"))
        );
        let mut lex = Token::lexer("#2017-02-26T23:50:30.120-3");
        assert_eq!(
            lex.next(),
            Some(Token::IsoDate("#2017-02-26T23:50:30.120-3"))
        );
        let mut lex = Token::lexer("#2017-02-26T23:50:30.120+3");
        assert_eq!(
            lex.next(),
            Some(Token::IsoDate("#2017-02-26T23:50:30.120+3"))
        );
        let mut lex = Token::lexer("#2017-02-26T23:50:30.120+3:30");
        assert_eq!(
            lex.next(),
            Some(Token::IsoDate("#2017-02-26T23:50:30.120+3:30"))
        );
    }

    #[test]
    fn test_regex() {
        let mut lex = Token::lexer("\"aaabbb\" =~ #/(a|b)+/#");
        assert_eq!(lex.next(), Some(Token::String("aaabbb")));
        assert_eq!(lex.next(), Some(Token::Matches));
        assert_eq!(lex.next(), Some(Token::RegExp("(a|b)+")));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer("\"aaabbb\" =~ #`(a|b|/)+`#");
        assert_eq!(lex.next(), Some(Token::String("aaabbb")));
        assert_eq!(lex.next(), Some(Token::Matches));

        assert_eq!(lex.next(), Some(Token::RegExp("(a|b|/)+")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_strings() {
        let mut lex = Token::lexer("msj = f\"\nIngrese su apuesta (min: ${apuesta-minima}, max: ${pozo}, 0 para finalizar juego): \"");
        assert_eq!(lex.next(), Some(Token::Id("msj")));
        assert_eq!(lex.next(), Some(Token::Assign));
        assert_eq!(lex.next(), Some(Token::FormatString("\nIngrese su apuesta (min: ${apuesta-minima}, max: ${pozo}, 0 para finalizar juego): ")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_chars() {
        let mut lex = Token::lexer("'a' 'b' '\\n' '\\u3829'");
        assert_eq!(lex.next(), Some(Token::Char("a")));
        assert_eq!(lex.next(), Some(Token::Char("b")));
        assert_eq!(lex.next(), Some(Token::Char("\\n")));
        assert_eq!(lex.next(), Some(Token::Char("\\u3829")));
        assert_eq!(lex.next(), None);
    }
}
