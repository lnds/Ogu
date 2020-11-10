use crate::lexer::tokens::Symbol::{LBRACKET, LCURLY, LPAREN, RBRACKET, RCURLY, RPAREN};
use logos::{Lexer, Logos};
use std::fmt::Display;

pub type LineSize = usize;

pub type LineNumber = usize;

pub type IndentStack = Vec<LineSize>;

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Symbol<'a> {
    INDENT,
    DEDENT,
    #[error]
    ERROR,
    #[regex(r"[ \t]", logos::skip)]
    WS,
    #[regex(r"--.*", logos::skip)]
    COMMENT,
    #[regex(r"[\n\r]+")]
    NL,
    #[token("as", priority = 2000)]
    AS,
    #[token("case", priority = 2000)]
    CASE,
    #[token("class", priority = 2000)]
    CLASS,
    #[token("cond", priority = 2000)]
    COND,
    #[token("do", priority = 2000)]
    DO,
    #[token("eager", priority = 2000)]
    EAGER,
    #[token("elif", priority = 2000)]
    ELIF,
    #[token("else", priority = 2000)]
    ELSE,
    #[token("exposing", priority = 2000)]
    EXPOSING,
    #[token("extends", priority = 2000)]
    EXTENDS,
    #[token("false", priority = 2000)]
    FALSE,
    #[token("for", priority = 2000)]
    FOR,
    #[token("from", priority = 2000)]
    FROM,
    #[token("handle", priority = 2000)]
    HANDLE,
    #[token("if", priority = 2000)]
    IF,
    #[token("import", priority = 2000)]
    IMPORT,
    #[token("in", priority = 2000)]
    IN,
    #[token("is", priority = 2000)]
    IS,
    #[token("lazy", priority = 2000)]
    LAZY,
    #[token("let", priority = 2000)]
    LET,
    #[token("loop", priority = 2000)]
    LOOP,
    #[token("module", priority = 2000)]
    MODULE,
    #[token("not", priority = 2000)]
    NOT,
    #[token("of", priority = 2000)]
    OF,
    #[token("otherwise", priority = 2000)]
    OTHERWISE,
    #[token("record", priority = 2000)]
    RECORD,
    #[token("recur", priority = 2000)]
    RECUR,
    #[token("reify", priority = 2000)]
    REIFY,
    #[token("repeat", priority = 2000)]
    REPEAT,
    #[token("return", priority = 2000)]
    RETURN,
    #[token("then", priority = 2000)]
    THEN,
    #[token("trait", priority = 2000)]
    TRAIT,
    #[token("until", priority = 2000)]
    UNTIL,
    #[token("when", priority = 2000)]
    WHEN,
    #[token("where", priority = 2000)]
    WHERE,
    #[token("while", priority = 2000)]
    WHILE,
    #[token("with", priority = 2000)]
    WITH,
    #[token("yield", priority = 2000)]
    YIELD,
    #[token("&&", priority = 1000)]
    AND,
    #[token("&", priority = 1000)]
    ANDB,
    #[token("@", priority = 1000)]
    ARROBA,
    #[token("->", priority = 1000)]
    ARROW,
    #[token("=", priority = 1000)]
    ASSIGN,
    #[token("<-", priority = 1000)]
    BACKARROW,
    #[token(":", priority = 1000)]
    COLON,
    #[token(",", priority = 1000)]
    COMMA,
    #[token(">>", priority = 1000)]
    COMPOSEFORWARD,
    #[token("<<", priority = 1000)]
    COMPOSEBACKWARD,
    #[token("::", priority = 1000)]
    CONS,
    #[token("/", priority = 1000)]
    DIV,
    #[token("//", priority = 1010)]
    DIVDIV,
    #[token("$", priority = 1000)]
    DOLLAR,
    #[token("...", priority = 1000)]
    DOTDOTDOT,
    #[token("..<", priority = 1000)]
    DOTDOTLESS,
    #[token("..", priority = 1000)]
    DOTDOT,
    #[token(".", priority = 1000)]
    DOT,
    #[token("!>", priority = 1000)]
    DOTO,
    #[token("<!", priority = 1000)]
    DOTOBACK,
    #[token("==", priority = 1000)]
    EQUALS,
    #[token(">=", priority = 1000)]
    GE,
    #[token(">", priority = 1000)]
    GT,
    #[token("|", priority = 1000)]
    GUARD,
    #[token("\\", priority = 1000)]
    LAMBDA,
    #[token("<=", priority = 1000)]
    LE,
    #[token("[", priority = 1000)]
    LBRACKET,
    #[token("{", priority = 1000)]
    LCURLY,
    #[token("(", priority = 1000)]
    LPAREN,
    #[token("<", priority = 1000)]
    LT,
    #[token("~", priority = 1000)]
    MATCH,
    #[token("=~", priority = 1000)]
    MATCHES,
    #[token("-", priority = 1000)]
    MINUS,
    #[token("%", priority = 1000)]
    MOD,
    #[token("*", priority = 1000)]
    MULT,
    #[token("/=", priority = 1000)]
    NOTEQUALS,
    #[token("||", priority = 1000)]
    OR,
    #[token("<|", priority = 1000)]
    PIPELEFT,
    #[token("|<", priority = 1000)]
    PIPELEFTFIRSTARG,
    #[token("|>")]
    PIPERIGHT,
    #[token(">|")]
    PIPERIGHTFIRSTARG,
    #[token("+", priority = 1000)]
    PLUS,
    #[token("++", priority = 1000)]
    PLUSPLUS,
    #[token("^")]
    POW,
    #[token("?", priority = 1000)]
    QUESTION,
    #[token("]", priority = 1000)]
    RBRACKET,
    #[token("}", priority = 1000)]
    RCURLY,
    #[token(")", priority = 1000)]
    RPAREN,
    #[regex(r"[A-Z][_a-zA-Z0-9]*", priority = 110, callback = extract_slice)]
    TID(&'a str),
    #[regex(r"[_a-zA-Z\-\+\*\$<>=][_a-zA-Z0-9\-\+\*\$<>=]*[!\?']*", priority = 100, callback = extract_slice)]
    ID(&'a str),
    #[regex(r#""([^"]*)""#, priority = 20, callback = extract_string)]
    STRING(&'a str),
    #[regex(r#"f"([^"]*)""#, priority = 20, callback = extract_f_string)]
    FSTRING(&'a str),
    #[regex(r"[\+\-]?[0-9]+[N]?", priority = 2000, callback = extract_slice)]
    INTEGER(&'a str),
    #[regex(r"[\+\-]?[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?[M]?", priority = 2000, callback = extract_slice)]
    FLOAT(&'a str),
    #[regex(r"[\+\-]?[0-9]+/[0-9]+[M]?", priority = 2000, callback = extract_slice)]
    RATIO(&'a str),
    #[regex(r"#(\d+)-(\d+)-(\d+)(T(\d+):(\d+)(:(\d+)(\.(\d+))?)?(Z|([+-]\d+(:\d+)?))?)?", callback = extract_slice)]
    ISODATE(&'a str),
    #[regex(r"#((/[^/]*/)|(\?[^?]*\?))", callback = extract_slice_from_1)]
    REGEX(&'a str),
    #[regex(r#"'(.|\\n|\\r|\\t|\\u[0-9]+)'"#, callback = extract_string)]
    CHAR(&'a str),
}

impl<'a> Symbol<'a> {
    pub fn is_open_paren(&self) -> bool {
        *self == LPAREN || *self == LBRACKET || *self == LCURLY
    }

    pub fn is_close_paren(&self) -> bool {
        *self == RPAREN || *self == RBRACKET || *self == RCURLY
    }
}

impl<'a> Display for Symbol<'a> {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> { todo!() }
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

fn extract_slice_from_1<'a>(lex: &mut Lexer<'a, Symbol<'a>>) -> Option<&'a str> {
    let slice = lex.slice();
    Some(&slice[1..])
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
        assert_eq!(lex.next(), Some(Symbol::LPAREN));
        assert_eq!(lex.next(), Some(Symbol::ID("a+b")));
        assert_eq!(lex.slice(), "a+b");
        assert_eq!(lex.next(), Some(Symbol::RPAREN));
    }

    #[test]
    fn test_opers() {
        let mut lex = Symbol::lexer("(a + b)");
        assert_eq!(lex.next(), Some(Symbol::LPAREN));
        assert_eq!(lex.next(), Some(Symbol::ID("a")));
        assert_eq!(lex.slice(), "a");
        assert_eq!(lex.next(), Some(Symbol::PLUS));
        assert_eq!(lex.next(), Some(Symbol::ID("b")));
        assert_eq!(lex.slice(), "b");
        assert_eq!(lex.next(), Some(Symbol::RPAREN));
    }

    #[test]
    fn test_string() {
        let mut lex = Symbol::lexer("\"ab\" ++ \"cd\"");
        assert_eq!(lex.next(), Some(Symbol::STRING("ab")));
        assert_eq!(lex.next(), Some(Symbol::PLUSPLUS));
        assert_eq!(lex.next(), Some(Symbol::STRING("cd")));
    }

    #[test]
    fn test_comments() {
        let mut lex = Symbol::lexer("ab ++ cd -- with comments");
        assert_eq!(lex.next(), Some(Symbol::ID("ab")));
        assert_eq!(lex.next(), Some(Symbol::PLUSPLUS));
        assert_eq!(lex.next(), Some(Symbol::ID("cd")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_ids() {
        let mut lex = Symbol::lexer("a.b");
        assert_eq!(lex.next(), Some(Symbol::ID("a")));
        assert_eq!(lex.next(), Some(Symbol::DOT));
        assert_eq!(lex.next(), Some(Symbol::ID("b")));
        assert_eq!(lex.next(), None);
        let mut lex = Symbol::lexer("Type.b");
        assert_eq!(lex.next(), Some(Symbol::TID("Type")));
        assert_eq!(lex.next(), Some(Symbol::DOT));
        assert_eq!(lex.next(), Some(Symbol::ID("b")));
        assert_eq!(lex.next(), None);
        let mut lex = Symbol::lexer("Type.a+b");
        assert_eq!(lex.next(), Some(Symbol::TID("Type")));
        assert_eq!(lex.next(), Some(Symbol::DOT));
        assert_eq!(lex.next(), Some(Symbol::ID("a+b")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("Type.**weird-id**");
        assert_eq!(lex.next(), Some(Symbol::TID("Type")));
        assert_eq!(lex.next(), Some(Symbol::DOT));
        assert_eq!(lex.next(), Some(Symbol::ID("**weird-id**")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("Type.**weird-id**.<name>.$value.bang!.question?");
        assert_eq!(lex.next(), Some(Symbol::TID("Type")));
        assert_eq!(lex.next(), Some(Symbol::DOT));
        assert_eq!(lex.next(), Some(Symbol::ID("**weird-id**")));
        assert_eq!(lex.next(), Some(Symbol::DOT));
        assert_eq!(lex.next(), Some(Symbol::ID("<name>")));
        assert_eq!(lex.next(), Some(Symbol::DOT));
        assert_eq!(lex.next(), Some(Symbol::ID("$value")));
        assert_eq!(lex.next(), Some(Symbol::DOT));
        assert_eq!(lex.next(), Some(Symbol::ID("bang!")));
        assert_eq!(lex.next(), Some(Symbol::DOT));
        assert_eq!(lex.next(), Some(Symbol::ID("question?")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_keywords() {
        let mut lex = Symbol::lexer("as class cond  do eager elif else");
        assert_eq!(lex.next(), Some(Symbol::AS));
        assert_eq!(lex.next(), Some(Symbol::CLASS));
        assert_eq!(lex.next(), Some(Symbol::COND));
        assert_eq!(lex.next(), Some(Symbol::DO));
        assert_eq!(lex.next(), Some(Symbol::EAGER));
        assert_eq!(lex.next(), Some(Symbol::ELIF));
        assert_eq!(lex.next(), Some(Symbol::ELSE));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("extends false for from handle if in is lazy");
        assert_eq!(lex.next(), Some(Symbol::EXTENDS));
        assert_eq!(lex.next(), Some(Symbol::FALSE));
        assert_eq!(lex.next(), Some(Symbol::FOR));
        assert_eq!(lex.next(), Some(Symbol::FROM));
        assert_eq!(lex.next(), Some(Symbol::HANDLE));
        assert_eq!(lex.next(), Some(Symbol::IF));
        assert_eq!(lex.next(), Some(Symbol::IN));
        assert_eq!(lex.next(), Some(Symbol::IS));
        assert_eq!(lex.next(), Some(Symbol::LAZY));
        assert_eq!(lex.next(), None);

        let mut lex =
            Symbol::lexer("let loop module otherwise record recur reify repeat then until");
        assert_eq!(lex.next(), Some(Symbol::LET));
        assert_eq!(lex.next(), Some(Symbol::LOOP));
        assert_eq!(lex.next(), Some(Symbol::MODULE));
        assert_eq!(lex.next(), Some(Symbol::OTHERWISE));
        assert_eq!(lex.next(), Some(Symbol::RECORD));
        assert_eq!(lex.next(), Some(Symbol::RECUR));
        assert_eq!(lex.next(), Some(Symbol::REIFY));
        assert_eq!(lex.next(), Some(Symbol::REPEAT));
        assert_eq!(lex.next(), Some(Symbol::THEN));
        assert_eq!(lex.next(), Some(Symbol::UNTIL));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("when where with");
        assert_eq!(lex.next(), Some(Symbol::WHEN));
        assert_eq!(lex.next(), Some(Symbol::WHERE));
        assert_eq!(lex.next(), Some(Symbol::WITH));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_ops() {
        let mut lex = Symbol::lexer("&& & @ -> = <- : , >>");
        assert_eq!(lex.next(), Some(Symbol::AND));
        assert_eq!(lex.next(), Some(Symbol::ANDB));
        assert_eq!(lex.next(), Some(Symbol::ARROBA));
        assert_eq!(lex.next(), Some(Symbol::ARROW));
        assert_eq!(lex.next(), Some(Symbol::ASSIGN));
        assert_eq!(lex.next(), Some(Symbol::BACKARROW));
        assert_eq!(lex.next(), Some(Symbol::COLON));
        assert_eq!(lex.next(), Some(Symbol::COMMA));
        assert_eq!(lex.next(), Some(Symbol::COMPOSEFORWARD));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("<< :: / // $ ... ..< .. . !>");
        assert_eq!(lex.next(), Some(Symbol::COMPOSEBACKWARD));
        assert_eq!(lex.next(), Some(Symbol::CONS));
        assert_eq!(lex.next(), Some(Symbol::DIV));
        assert_eq!(lex.next(), Some(Symbol::DIVDIV));
        assert_eq!(lex.next(), Some(Symbol::DOLLAR));
        assert_eq!(lex.next(), Some(Symbol::DOTDOTDOT));
        assert_eq!(lex.next(), Some(Symbol::DOTDOTLESS));
        assert_eq!(lex.next(), Some(Symbol::DOTDOT));
        assert_eq!(lex.next(), Some(Symbol::DOT));
        assert_eq!(lex.next(), Some(Symbol::DOTO));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("<! == >= > | \\ <= [ { (");
        assert_eq!(lex.next(), Some(Symbol::DOTOBACK));
        assert_eq!(lex.next(), Some(Symbol::EQUALS));
        assert_eq!(lex.next(), Some(Symbol::GE));
        assert_eq!(lex.next(), Some(Symbol::GT));
        assert_eq!(lex.next(), Some(Symbol::GUARD));
        assert_eq!(lex.next(), Some(Symbol::LAMBDA));
        assert_eq!(lex.next(), Some(Symbol::LE));
        assert_eq!(lex.next(), Some(Symbol::LBRACKET));
        assert_eq!(lex.next(), Some(Symbol::LCURLY));
        assert_eq!(lex.next(), Some(Symbol::LPAREN));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("< ~ =~ - % * /= || <| |<");
        assert_eq!(lex.next(), Some(Symbol::LT));
        assert_eq!(lex.next(), Some(Symbol::MATCH));
        assert_eq!(lex.next(), Some(Symbol::MATCHES));
        assert_eq!(lex.next(), Some(Symbol::MINUS));
        assert_eq!(lex.next(), Some(Symbol::MOD));
        assert_eq!(lex.next(), Some(Symbol::MULT));
        assert_eq!(lex.next(), Some(Symbol::NOTEQUALS));
        assert_eq!(lex.next(), Some(Symbol::OR));
        assert_eq!(lex.next(), Some(Symbol::PIPELEFT));
        assert_eq!(lex.next(), Some(Symbol::PIPELEFTFIRSTARG));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("|> >| + ++ ^ ? ] } )");
        assert_eq!(lex.next(), Some(Symbol::PIPERIGHT));
        assert_eq!(lex.next(), Some(Symbol::PIPERIGHTFIRSTARG));
        assert_eq!(lex.next(), Some(Symbol::PLUS));
        assert_eq!(lex.next(), Some(Symbol::PLUSPLUS));
        assert_eq!(lex.next(), Some(Symbol::POW));
        assert_eq!(lex.next(), Some(Symbol::QUESTION));
        assert_eq!(lex.next(), Some(Symbol::RBRACKET));
        assert_eq!(lex.next(), Some(Symbol::RCURLY));
        assert_eq!(lex.next(), Some(Symbol::RPAREN));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_literals() {
        let mut lex = Symbol::lexer("2 + 2");
        assert_eq!(lex.next(), Some(Symbol::INTEGER("2")));
        assert_eq!(lex.next(), Some(Symbol::PLUS));
        assert_eq!(lex.next(), Some(Symbol::INTEGER("2")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("1234567890 123.4567890");
        assert_eq!(lex.next(), Some(Symbol::INTEGER("1234567890")));
        assert_eq!(lex.next(), Some(Symbol::FLOAT("123.4567890")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("12/34 1/3 2.45E10 3.4e-20");
        assert_eq!(lex.next(), Some(Symbol::RATIO("12/34")));
        assert_eq!(lex.next(), Some(Symbol::RATIO("1/3")));
        assert_eq!(lex.next(), Some(Symbol::FLOAT("2.45E10")));
        assert_eq!(lex.next(), Some(Symbol::FLOAT("3.4e-20")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("-1/4 +1/4 -0.4 +0.4 +32 -32 .333 -.455");
        assert_eq!(lex.next(), Some(Symbol::RATIO("-1/4")));
        assert_eq!(lex.next(), Some(Symbol::RATIO("+1/4")));
        assert_eq!(lex.next(), Some(Symbol::FLOAT("-0.4")));
        assert_eq!(lex.next(), Some(Symbol::FLOAT("+0.4")));
        assert_eq!(lex.next(), Some(Symbol::INTEGER("+32")));
        assert_eq!(lex.next(), Some(Symbol::INTEGER("-32")));
        assert_eq!(lex.next(), Some(Symbol::FLOAT(".333")));
        assert_eq!(lex.next(), Some(Symbol::FLOAT("-.455")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("2.4M 200N");
        assert_eq!(lex.next(), Some(Symbol::FLOAT("2.4M")));
        assert_eq!(lex.next(), Some(Symbol::INTEGER("200N")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_ranges() {
        let mut lex = Symbol::lexer("a..b");
        assert_eq!(lex.next(), Some(Symbol::ID("a")));
        assert_eq!(lex.next(), Some(Symbol::DOTDOT));
        assert_eq!(lex.next(), Some(Symbol::ID("b")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("1..2");
        assert_eq!(lex.next(), Some(Symbol::INTEGER("1")));
        assert_eq!(lex.next(), Some(Symbol::DOTDOT));
        assert_eq!(lex.next(), Some(Symbol::INTEGER("2")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("1..<2");
        assert_eq!(lex.next(), Some(Symbol::INTEGER("1")));
        assert_eq!(lex.next(), Some(Symbol::DOTDOTLESS));
        assert_eq!(lex.next(), Some(Symbol::INTEGER("2")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("1...");
        assert_eq!(lex.next(), Some(Symbol::INTEGER("1")));
        assert_eq!(lex.next(), Some(Symbol::DOTDOTDOT));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("1.4..3.4");
        assert_eq!(lex.next(), Some(Symbol::FLOAT("1.4")));
        assert_eq!(lex.next(), Some(Symbol::DOTDOT));
        assert_eq!(lex.next(), Some(Symbol::FLOAT("3.4")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_indent() {
        let mut lex = Symbol::lexer("  a");
        assert_eq!(lex.next(), Some(Symbol::ID("a")));
    }

    #[test]
    fn test_some_expressions() {
        let mut lex = Symbol::lexer("\"a b c d\" |> .toUpperCase |> .split \" \" |> first");
        assert_eq!(lex.next(), Some(Symbol::STRING("a b c d")));
        assert_eq!(lex.next(), Some(Symbol::PIPERIGHT));
        assert_eq!(lex.next(), Some(Symbol::DOT));
        assert_eq!(lex.next(), Some(Symbol::ID("toUpperCase")));
        assert_eq!(lex.next(), Some(Symbol::PIPERIGHT));
        assert_eq!(lex.next(), Some(Symbol::DOT));
        assert_eq!(lex.next(), Some(Symbol::ID("split")));
        assert_eq!(lex.next(), Some(Symbol::STRING(" ")));
        assert_eq!(lex.next(), Some(Symbol::PIPERIGHT));
        assert_eq!(lex.next(), Some(Symbol::ID("first")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_dates() {
        let mut lex = Symbol::lexer("#2017-02-26");
        assert_eq!(lex.next(), Some(Symbol::ISODATE("#2017-02-26")));
        let mut lex = Symbol::lexer("#2017-02-26T23:50");
        assert_eq!(lex.next(), Some(Symbol::ISODATE("#2017-02-26T23:50")));
        let mut lex = Symbol::lexer("#2017-02-26T23:50:30");
        assert_eq!(lex.next(), Some(Symbol::ISODATE("#2017-02-26T23:50:30")));
        let mut lex = Symbol::lexer("#2017-02-26T23:50:30.120");
        assert_eq!(
            lex.next(),
            Some(Symbol::ISODATE("#2017-02-26T23:50:30.120"))
        );
        let mut lex = Symbol::lexer("#2017-02-26T23:50:30.120Z");
        assert_eq!(
            lex.next(),
            Some(Symbol::ISODATE("#2017-02-26T23:50:30.120Z"))
        );
        let mut lex = Symbol::lexer("#2017-02-26T23:50:30.120-3");
        assert_eq!(
            lex.next(),
            Some(Symbol::ISODATE("#2017-02-26T23:50:30.120-3"))
        );
        let mut lex = Symbol::lexer("#2017-02-26T23:50:30.120+3");
        assert_eq!(
            lex.next(),
            Some(Symbol::ISODATE("#2017-02-26T23:50:30.120+3"))
        );
        let mut lex = Symbol::lexer("#2017-02-26T23:50:30.120+3:30");
        assert_eq!(
            lex.next(),
            Some(Symbol::ISODATE("#2017-02-26T23:50:30.120+3:30"))
        );
    }

    #[test]
    fn test_regex() {
        let mut lex = Symbol::lexer("\"aaabbb\" =~ #/(a|b)+/");
        assert_eq!(lex.next(), Some(Symbol::STRING("aaabbb")));
        assert_eq!(lex.next(), Some(Symbol::MATCHES));
        assert_eq!(lex.next(), Some(Symbol::REGEX("/(a|b)+/")));
        assert_eq!(lex.next(), None);

        let mut lex = Symbol::lexer("\"aaabbb\" =~ #?(a|b|/)+?");
        assert_eq!(lex.next(), Some(Symbol::STRING("aaabbb")));
        assert_eq!(lex.next(), Some(Symbol::MATCHES));
        assert_eq!(lex.next(), Some(Symbol::REGEX("?(a|b|/)+?")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_strings() {
        let mut lex = Symbol::lexer("msj = f\"\nIngrese su apuesta (min: ${apuesta-minima}, max: ${pozo}, 0 para finalizar juego): \"");
        assert_eq!(lex.next(), Some(Symbol::ID("msj")));
        assert_eq!(lex.next(), Some(Symbol::ASSIGN));
        assert_eq!(lex.next(), Some(Symbol::FSTRING("\nIngrese su apuesta (min: ${apuesta-minima}, max: ${pozo}, 0 para finalizar juego): ")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_chars() {
        let mut lex = Symbol::lexer("'a' 'b' '\\n' '\\u3829'");
        assert_eq!(lex.next(), Some(Symbol::CHAR("a")));
        assert_eq!(lex.next(), Some(Symbol::CHAR("b")));
        assert_eq!(lex.next(), Some(Symbol::CHAR("\\n")));
        assert_eq!(lex.next(), Some(Symbol::CHAR("\\u3829")));
        assert_eq!(lex.next(), None);
    }
}
