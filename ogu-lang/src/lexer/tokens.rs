use std::fmt::Display;

use logos::{Lexer, Logos};

pub(crate) type LineCount = usize;

pub(crate) type LineWidth = usize;
pub(crate) type LineNumber = usize;

pub(crate) type IndentStack = Vec<LineCount>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Token<'a> {
    pub(crate) lexeme: Lexeme<'a>,
    pub(crate) line: LineCount,
    pub(crate) col: LineWidth,
}

impl<'a> Token<'a> {
    pub(crate) fn new(lexeme: Lexeme<'a>, line: LineCount, col: LineWidth) -> Self {
        Token { lexeme, line, col }
    }
}

pub(crate) type TokenList<'a> = Vec<Token<'a>>;

#[derive(Logos, Debug, Clone, Copy, PartialEq)]
pub(crate) enum Lexeme<'a> {
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
    #[token("f32", priority = 2000)]
    TyF32,
    #[token("f64", priority = 2000)]
    TyF64,
    #[token("false", priority = 2000)]
    False,
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
    #[token("!", priority = 2000)]
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
    #[token("resume", priority = 2000)]
    Resume,
    #[token("then", priority = 2000)]
    Then,
    #[token("trait", priority = 2000)]
    Trait,
    #[token("true", priority = 2000)]
    True,
    #[token("try", priority = 2000)]
    Try,
    #[token("type", priority = 2000)]
    Type,
    #[token("where", priority = 2000)]
    Where,
    #[token("with", priority = 2000)]
    With,
    #[token("yield", priority = 2000)]
    Yield,
    #[token("&&", priority = 1000)]
    And,
    #[token("@", priority = 1000)]
    At,
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
    #[token("-", priority = 1000)]
    Minus,
    #[token("%", priority = 1000)]
    Mod,
    #[token("*", priority = 1000)]
    Mult,
    #[token("!=", priority = 1000)]
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
    #[regex(r"[\+\-]?[0-9]+[N|i8|i16|i32|i128|isize|u8|u16|u32|u64|u128|usize]?", priority = 2000, callback = extract_slice)]
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

impl<'a> Lexeme<'a> {
    pub(crate) fn is_open_paren(&self) -> bool {
        matches!(
            *self,
            Lexeme::LeftParen
                | Lexeme::LeftBracket
                | Lexeme::LeftCurly
                | Lexeme::LeftCurlyCurly
                | Lexeme::HashCurly
        )
    }

    pub(crate) fn is_close_paren(&self) -> bool {
        matches!(
            *self,
            Lexeme::RightParen
                | Lexeme::RightBracket
                | Lexeme::RightCurly
                | Lexeme::RightCurlyCurly
        )
    }
}

impl<'a> Display for Lexeme<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{:?}", self)
    }
}

fn extract_string<'a>(lex: &mut Lexer<'a, Lexeme<'a>>) -> &'a str {
    let slice = lex.slice();
    &slice[1..slice.len() - 1]
}

fn extract_f_string<'a>(lex: &mut Lexer<'a, Lexeme<'a>>) -> &'a str {
    let slice = lex.slice();
    &slice[2..slice.len() - 1]
}

fn extract_slice<'a>(lex: &mut Lexer<'a, Lexeme<'a>>) -> &'a str {
    lex.slice()
}

fn extract_regex<'a>(lex: &mut Lexer<'a, Lexeme<'a>>) -> &'a str {
    let slice = lex.slice();
    &slice[2..slice.len() - 2]
}

#[cfg(test)]
mod test_tokens {
    use logos::Logos;

    use crate::lexer::tokens::Lexeme;

    #[test]
    fn test_symbols() {
        let mut lex = Lexeme::lexer("(a+b)");
        assert_eq!(lex.next(), Some(Lexeme::LeftParen));
        assert_eq!(lex.next(), Some(Lexeme::Id("a+b")));
        assert_eq!(lex.slice(), "a+b");
        assert_eq!(lex.next(), Some(Lexeme::RightParen));
    }

    #[test]
    fn test_opers() {
        let mut lex = Lexeme::lexer("(a + b)");
        assert_eq!(lex.next(), Some(Lexeme::LeftParen));
        assert_eq!(lex.next(), Some(Lexeme::Id("a")));
        assert_eq!(lex.slice(), "a");
        assert_eq!(lex.next(), Some(Lexeme::Plus));
        assert_eq!(lex.next(), Some(Lexeme::Id("b")));
        assert_eq!(lex.slice(), "b");
        assert_eq!(lex.next(), Some(Lexeme::RightParen));
    }

    #[test]
    fn test_string() {
        let mut lex = Lexeme::lexer("\"ab\" ++ \"cd\"");
        assert_eq!(lex.next(), Some(Lexeme::String("ab")));
        assert_eq!(lex.next(), Some(Lexeme::PlusPlus));
        assert_eq!(lex.next(), Some(Lexeme::String("cd")));
    }

    #[test]
    fn test_comments() {
        let mut lex = Lexeme::lexer("ab ++ cd -- with comments");
        assert_eq!(lex.next(), Some(Lexeme::Id("ab")));
        assert_eq!(lex.next(), Some(Lexeme::PlusPlus));
        assert_eq!(lex.next(), Some(Lexeme::Id("cd")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_ids() {
        let mut lex = Lexeme::lexer("a.b");
        assert_eq!(lex.next(), Some(Lexeme::Id("a")));
        assert_eq!(lex.next(), Some(Lexeme::Dot));
        assert_eq!(lex.next(), Some(Lexeme::Id("b")));
        assert_eq!(lex.next(), None);
        let mut lex = Lexeme::lexer("Type.b");
        assert_eq!(lex.next(), Some(Lexeme::TypeId("Type")));
        assert_eq!(lex.next(), Some(Lexeme::Dot));
        assert_eq!(lex.next(), Some(Lexeme::Id("b")));
        assert_eq!(lex.next(), None);
        let mut lex = Lexeme::lexer("Type.a+b");
        assert_eq!(lex.next(), Some(Lexeme::TypeId("Type")));
        assert_eq!(lex.next(), Some(Lexeme::Dot));
        assert_eq!(lex.next(), Some(Lexeme::Id("a+b")));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("Type.**weird-id**");
        assert_eq!(lex.next(), Some(Lexeme::TypeId("Type")));
        assert_eq!(lex.next(), Some(Lexeme::Dot));
        assert_eq!(lex.next(), Some(Lexeme::Id("**weird-id**")));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("Type.**weird-id**.name.$value.bang!.question?");
        assert_eq!(lex.next(), Some(Lexeme::TypeId("Type")));
        assert_eq!(lex.next(), Some(Lexeme::Dot));
        assert_eq!(lex.next(), Some(Lexeme::Id("**weird-id**")));
        assert_eq!(lex.next(), Some(Lexeme::Dot));
        assert_eq!(lex.next(), Some(Lexeme::Id("name")));
        assert_eq!(lex.next(), Some(Lexeme::Dot));
        assert_eq!(lex.next(), Some(Lexeme::Id("$value")));
        assert_eq!(lex.next(), Some(Lexeme::Dot));
        assert_eq!(lex.next(), Some(Lexeme::Id("bang!")));
        assert_eq!(lex.next(), Some(Lexeme::Dot));
        assert_eq!(lex.next(), Some(Lexeme::Id("question?")));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("id lost+found");
        assert_eq!(lex.next(), Some(Lexeme::Id("id")));
        assert_eq!(lex.next(), Some(Lexeme::Id("lost+found")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_keywords() {
        let mut lex = Lexeme::lexer("as case cond do derive eager effect elif else");
        assert_eq!(lex.next(), Some(Lexeme::As));
        assert_eq!(lex.next(), Some(Lexeme::Case));
        assert_eq!(lex.next(), Some(Lexeme::Cond));
        assert_eq!(lex.next(), Some(Lexeme::Do));
        assert_eq!(lex.next(), Some(Lexeme::Derive));
        assert_eq!(lex.next(), Some(Lexeme::Eager));
        assert_eq!(lex.next(), Some(Lexeme::Effect));
        assert_eq!(lex.next(), Some(Lexeme::Elif));
        assert_eq!(lex.next(), Some(Lexeme::Else));
        assert_eq!(lex.next(), None);

        let mut lex =
            Lexeme::lexer("exposing extends extern false for from if handle handler in is lazy");
        assert_eq!(lex.next(), Some(Lexeme::Exposing));
        assert_eq!(lex.next(), Some(Lexeme::Extends));
        assert_eq!(lex.next(), Some(Lexeme::Extern));
        assert_eq!(lex.next(), Some(Lexeme::False));
        assert_eq!(lex.next(), Some(Lexeme::For));
        assert_eq!(lex.next(), Some(Lexeme::From));
        assert_eq!(lex.next(), Some(Lexeme::If));
        assert_eq!(lex.next(), Some(Lexeme::Handle));
        assert_eq!(lex.next(), Some(Lexeme::Handler));
        assert_eq!(lex.next(), Some(Lexeme::In));
        assert_eq!(lex.next(), Some(Lexeme::Is));
        assert_eq!(lex.next(), Some(Lexeme::Lazy));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer(
            "let loop macro module of otherwise perform primitive repeat recur reify resume then trait true try",
        );
        assert_eq!(lex.next(), Some(Lexeme::Let));
        assert_eq!(lex.next(), Some(Lexeme::Loop));
        assert_eq!(lex.next(), Some(Lexeme::Macro));
        assert_eq!(lex.next(), Some(Lexeme::Module));
        assert_eq!(lex.next(), Some(Lexeme::Of));
        assert_eq!(lex.next(), Some(Lexeme::Otherwise));
        assert_eq!(lex.next(), Some(Lexeme::Perform));
        assert_eq!(lex.next(), Some(Lexeme::Primitive));
        assert_eq!(lex.next(), Some(Lexeme::Repeat));
        assert_eq!(lex.next(), Some(Lexeme::Recur));
        assert_eq!(lex.next(), Some(Lexeme::Reify));
        assert_eq!(lex.next(), Some(Lexeme::Resume));
        assert_eq!(lex.next(), Some(Lexeme::Then));
        assert_eq!(lex.next(), Some(Lexeme::Trait));
        assert_eq!(lex.next(), Some(Lexeme::True));
        assert_eq!(lex.next(), Some(Lexeme::Try));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("type where with yield");
        assert_eq!(lex.next(), Some(Lexeme::Type));
        assert_eq!(lex.next(), Some(Lexeme::Where));
        assert_eq!(lex.next(), Some(Lexeme::With));
        assert_eq!(lex.next(), Some(Lexeme::Yield));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_ops() {
        let mut lex = Lexeme::lexer("&&  @ -> = <- : , >> !");
        assert_eq!(lex.next(), Some(Lexeme::And));
        assert_eq!(lex.next(), Some(Lexeme::At));
        assert_eq!(lex.next(), Some(Lexeme::Arrow));
        assert_eq!(lex.next(), Some(Lexeme::Assign));
        assert_eq!(lex.next(), Some(Lexeme::BackArrow));
        assert_eq!(lex.next(), Some(Lexeme::Colon));
        assert_eq!(lex.next(), Some(Lexeme::Comma));
        assert_eq!(lex.next(), Some(Lexeme::ComposeForward));
        assert_eq!(lex.next(), Some(Lexeme::Not));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("<< :: / // $  .. . ");
        assert_eq!(lex.next(), Some(Lexeme::ComposeBackward));
        assert_eq!(lex.next(), Some(Lexeme::Cons));
        assert_eq!(lex.next(), Some(Lexeme::Div));
        assert_eq!(lex.next(), Some(Lexeme::DivDiv));
        assert_eq!(lex.next(), Some(Lexeme::Dollar));
        assert_eq!(lex.next(), Some(Lexeme::DotDot));
        assert_eq!(lex.next(), Some(Lexeme::Dot));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("== >= > | \\ <= [ { {{ #{ (");
        assert_eq!(lex.next(), Some(Lexeme::Equal));
        assert_eq!(lex.next(), Some(Lexeme::GreaterOrEqual));
        assert_eq!(lex.next(), Some(Lexeme::Greater));
        assert_eq!(lex.next(), Some(Lexeme::Guard));
        assert_eq!(lex.next(), Some(Lexeme::Lambda));
        assert_eq!(lex.next(), Some(Lexeme::LessThanOrEqual));
        assert_eq!(lex.next(), Some(Lexeme::LeftBracket));
        assert_eq!(lex.next(), Some(Lexeme::LeftCurly));
        assert_eq!(lex.next(), Some(Lexeme::LeftCurlyCurly));
        assert_eq!(lex.next(), Some(Lexeme::HashCurly));
        assert_eq!(lex.next(), Some(Lexeme::LeftParen));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("<  - % *  != || <|");
        assert_eq!(lex.next(), Some(Lexeme::LessThan));
        assert_eq!(lex.next(), Some(Lexeme::Minus));
        assert_eq!(lex.next(), Some(Lexeme::Mod));
        assert_eq!(lex.next(), Some(Lexeme::Mult));
        assert_eq!(lex.next(), Some(Lexeme::NotEqual));
        assert_eq!(lex.next(), Some(Lexeme::Or));
        assert_eq!(lex.next(), Some(Lexeme::PipeLeft));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("|> + ++ ^ ? ] } }} )");
        assert_eq!(lex.next(), Some(Lexeme::PipeRight));
        assert_eq!(lex.next(), Some(Lexeme::Plus));
        assert_eq!(lex.next(), Some(Lexeme::PlusPlus));
        assert_eq!(lex.next(), Some(Lexeme::Pow));
        assert_eq!(lex.next(), Some(Lexeme::Question));
        assert_eq!(lex.next(), Some(Lexeme::RightBracket));
        assert_eq!(lex.next(), Some(Lexeme::RightCurly));
        assert_eq!(lex.next(), Some(Lexeme::RightCurlyCurly));
        assert_eq!(lex.next(), Some(Lexeme::RightParen));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_literals() {
        let mut lex = Lexeme::lexer("2 + 2");
        assert_eq!(lex.next(), Some(Lexeme::Integer("2")));
        assert_eq!(lex.next(), Some(Lexeme::Plus));
        assert_eq!(lex.next(), Some(Lexeme::Integer("2")));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("1234567890 123.4567890");
        assert_eq!(lex.next(), Some(Lexeme::Integer("1234567890")));
        assert_eq!(lex.next(), Some(Lexeme::Float("123.4567890")));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("12/34 1/3 2.45E10 3.4e-20");
        assert_eq!(lex.next(), Some(Lexeme::Ratio("12/34")));
        assert_eq!(lex.next(), Some(Lexeme::Ratio("1/3")));
        assert_eq!(lex.next(), Some(Lexeme::Float("2.45E10")));
        assert_eq!(lex.next(), Some(Lexeme::Float("3.4e-20")));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("-1/4 +1/4 -0.4 +0.4 +32 -32 .333 -.455");
        assert_eq!(lex.next(), Some(Lexeme::Ratio("-1/4")));
        assert_eq!(lex.next(), Some(Lexeme::Ratio("+1/4")));
        assert_eq!(lex.next(), Some(Lexeme::Float("-0.4")));
        assert_eq!(lex.next(), Some(Lexeme::Float("+0.4")));
        assert_eq!(lex.next(), Some(Lexeme::Integer("+32")));
        assert_eq!(lex.next(), Some(Lexeme::Integer("-32")));
        assert_eq!(lex.next(), Some(Lexeme::Float(".333")));
        assert_eq!(lex.next(), Some(Lexeme::Float("-.455")));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("2.4M 200N");
        assert_eq!(lex.next(), Some(Lexeme::Float("2.4M")));
        assert_eq!(lex.next(), Some(Lexeme::Integer("200N")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_ranges() {
        let mut lex = Lexeme::lexer("a..b");
        assert_eq!(lex.next(), Some(Lexeme::Id("a")));
        assert_eq!(lex.next(), Some(Lexeme::DotDot));
        assert_eq!(lex.next(), Some(Lexeme::Id("b")));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("1..2");
        assert_eq!(lex.next(), Some(Lexeme::Integer("1")));
        assert_eq!(lex.next(), Some(Lexeme::DotDot));
        assert_eq!(lex.next(), Some(Lexeme::Integer("2")));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("1..2");
        assert_eq!(lex.next(), Some(Lexeme::Integer("1")));
        assert_eq!(lex.next(), Some(Lexeme::DotDot));
        assert_eq!(lex.next(), Some(Lexeme::Integer("2")));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("1..");
        assert_eq!(lex.next(), Some(Lexeme::Integer("1")));
        assert_eq!(lex.next(), Some(Lexeme::DotDot));
        assert_eq!(lex.next(), None);

        let mut lex = Lexeme::lexer("1.4..3.4");
        assert_eq!(lex.next(), Some(Lexeme::Float("1.4")));
        assert_eq!(lex.next(), Some(Lexeme::DotDot));
        assert_eq!(lex.next(), Some(Lexeme::Float("3.4")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_indent() {
        let mut lex = Lexeme::lexer("  a");
        assert_eq!(lex.next(), Some(Lexeme::Id("a")));
    }

    #[test]
    fn test_some_expressions() {
        let mut lex = Lexeme::lexer("\"a b c d\" |> .toUpperCase |> .split \" \" |> first");
        assert_eq!(lex.next(), Some(Lexeme::String("a b c d")));
        assert_eq!(lex.next(), Some(Lexeme::PipeRight));
        assert_eq!(lex.next(), Some(Lexeme::Dot));
        assert_eq!(lex.next(), Some(Lexeme::Id("toUpperCase")));
        assert_eq!(lex.next(), Some(Lexeme::PipeRight));
        assert_eq!(lex.next(), Some(Lexeme::Dot));
        assert_eq!(lex.next(), Some(Lexeme::Id("split")));
        assert_eq!(lex.next(), Some(Lexeme::String(" ")));
        assert_eq!(lex.next(), Some(Lexeme::PipeRight));
        assert_eq!(lex.next(), Some(Lexeme::Id("first")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_dates() {
        let mut lex = Lexeme::lexer("#2017-02-26");
        assert_eq!(lex.next(), Some(Lexeme::IsoDate("#2017-02-26")));
        let mut lex = Lexeme::lexer("#2017-02-26T23:50");
        assert_eq!(lex.next(), Some(Lexeme::IsoDate("#2017-02-26T23:50")));
        let mut lex = Lexeme::lexer("#2017-02-26T23:50:30");
        assert_eq!(lex.next(), Some(Lexeme::IsoDate("#2017-02-26T23:50:30")));
        let mut lex = Lexeme::lexer("#2017-02-26T23:50:30.120");
        assert_eq!(
            lex.next(),
            Some(Lexeme::IsoDate("#2017-02-26T23:50:30.120"))
        );
        let mut lex = Lexeme::lexer("#2017-02-26T23:50:30.120Z");
        assert_eq!(
            lex.next(),
            Some(Lexeme::IsoDate("#2017-02-26T23:50:30.120Z"))
        );
        let mut lex = Lexeme::lexer("#2017-02-26T23:50:30.120-3");
        assert_eq!(
            lex.next(),
            Some(Lexeme::IsoDate("#2017-02-26T23:50:30.120-3"))
        );
        let mut lex = Lexeme::lexer("#2017-02-26T23:50:30.120+3");
        assert_eq!(
            lex.next(),
            Some(Lexeme::IsoDate("#2017-02-26T23:50:30.120+3"))
        );
        let mut lex = Lexeme::lexer("#2017-02-26T23:50:30.120+3:30");
        assert_eq!(
            lex.next(),
            Some(Lexeme::IsoDate("#2017-02-26T23:50:30.120+3:30"))
        );
    }

    #[test]
    fn test_regex() {
        let mut lex = Lexeme::lexer("\"aaabbb\"  #`(a|b|/)+`#");
        assert_eq!(lex.next(), Some(Lexeme::String("aaabbb")));
        assert_eq!(lex.next(), Some(Lexeme::RegExp("(a|b|/)+")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_strings() {
        let mut lex = Lexeme::lexer("msj = f\"\nIngrese su apuesta (min: ${apuesta-minima}, max: ${pozo}, 0 para finalizar juego): \"");
        assert_eq!(lex.next(), Some(Lexeme::Id("msj")));
        assert_eq!(lex.next(), Some(Lexeme::Assign));
        assert_eq!(lex.next(), Some(Lexeme::FormatString("\nIngrese su apuesta (min: ${apuesta-minima}, max: ${pozo}, 0 para finalizar juego): ")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_chars() {
        let mut lex = Lexeme::lexer("'a' 'b' '\\n' '\\u3829'");
        assert_eq!(lex.next(), Some(Lexeme::Char("a")));
        assert_eq!(lex.next(), Some(Lexeme::Char("b")));
        assert_eq!(lex.next(), Some(Lexeme::Char("\\n")));
        assert_eq!(lex.next(), Some(Lexeme::Char("\\u3829")));
        assert_eq!(lex.next(), None);
    }
}
