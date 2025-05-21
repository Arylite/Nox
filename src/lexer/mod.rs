//! Lexer module for the language
//! This module defines the lexer that tokenizes the source code into a stream of tokens.
//! It uses the `logos` crate for efficient lexing and provides detailed token information
//! including source location tracking.

use logos::Logos;
use std::fmt;
use std::ops::Range;

/// Source location information for error reporting
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub length: usize,
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}

/// A token with its associated span and source location
#[derive(Debug, Clone, PartialEq)]
pub struct TokenWithLocation {
    pub token: Token,
    pub span: Range<usize>,
    pub location: SourceLocation,
}

/// Helper function to parse a string literal, handling escape sequences
fn parse_string_literal(s: &str) -> Result<String, String> {
    let s = &s[1..s.len() - 1]; // Remove quotes
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some('\'') => result.push('\''),
                Some('u') => {
                    // Unicode escape sequence \uXXXX
                    let hex: String = chars.by_ref().take(4).collect();
                    if hex.len() != 4 {
                        return Err(format!("Invalid Unicode escape sequence: \\u{}", hex));
                    }
                    match u32::from_str_radix(&hex, 16) {
                        Ok(code) => match char::from_u32(code) {
                            Some(unicode_char) => result.push(unicode_char),
                            None => return Err(format!("Invalid Unicode codepoint: \\u{}", hex)),
                        },
                        Err(_) => {
                            return Err(format!("Invalid Unicode escape sequence: \\u{}", hex));
                        }
                    }
                }
                Some(c) => return Err(format!("Unknown escape sequence: \\{}", c)),
                None => return Err("Unterminated escape sequence".to_string()),
            }
        } else {
            result.push(c);
        }
    }

    Ok(result)
}

/// Helper function to parse a numeric literal (integer or float)
fn parse_number(s: &str) -> Result<NumericLiteral, String> {
    // Check for hexadecimal
    if s.starts_with("0x") || s.starts_with("0X") {
        let value = u64::from_str_radix(&s[2..], 16)
            .map_err(|e| format!("Invalid hexadecimal literal: {}", e))?;
        return Ok(NumericLiteral::Integer(value as i64));
    }

    // Check for binary
    if s.starts_with("0b") || s.starts_with("0B") {
        let value = u64::from_str_radix(&s[2..], 2)
            .map_err(|e| format!("Invalid binary literal: {}", e))?;
        return Ok(NumericLiteral::Integer(value as i64));
    }

    // Check for octal
    if s.starts_with("0o") || s.starts_with("0O") {
        let value =
            u64::from_str_radix(&s[2..], 8).map_err(|e| format!("Invalid octal literal: {}", e))?;
        return Ok(NumericLiteral::Integer(value as i64));
    }

    // Check for float
    if s.contains('.') || s.contains('e') || s.contains('E') {
        let value = s
            .parse::<f64>()
            .map_err(|e| format!("Invalid float literal: {}", e))?;
        return Ok(NumericLiteral::Float(value));
    }

    // Regular integer
    let value = s
        .parse::<i64>()
        .map_err(|e| format!("Invalid integer literal: {}", e))?;
    Ok(NumericLiteral::Integer(value))
}

/// Helper function to parse a character literal
fn parse_char_literal(s: &str) -> Result<char, String> {
    // Remove surrounding quotes
    let content = &s[1..s.len() - 1];

    if content.is_empty() {
        return Err("Empty character literal".to_string());
    }

    let mut chars = content.chars();
    let first = chars.next().unwrap();

    if first == '\\' {
        // Escape sequence
        match chars.next() {
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            Some('\\') => Ok('\\'),
            Some('\'') => Ok('\''),
            Some('"') => Ok('"'),
            Some('0') => Ok('\0'),
            Some('u') => {
                // Unicode escape sequence \uXXXX
                let hex: String = chars.take(4).collect();
                if hex.len() != 4 {
                    return Err(format!("Invalid Unicode escape sequence: \\u{}", hex));
                }
                match u32::from_str_radix(&hex, 16) {
                    Ok(code) => match char::from_u32(code) {
                        Some(unicode_char) => Ok(unicode_char),
                        None => Err(format!("Invalid Unicode codepoint: \\u{}", hex)),
                    },
                    Err(_) => Err(format!("Invalid Unicode escape sequence: \\u{}", hex)),
                }
            }
            Some(c) => Err(format!("Unknown escape sequence: \\{}", c)),
            None => Err("Unterminated escape sequence".to_string()),
        }
    } else if chars.next().is_some() {
        // More than one character
        Err("Character literal contains more than one character".to_string())
    } else {
        // Single character
        Ok(first)
    }
}

/// Numeric literal variants to support both integers and floats
#[derive(Debug, Clone, PartialEq)]
pub enum NumericLiteral {
    Integer(i64),
    Float(f64),
}

/// Core token type for the language
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\f\r]+")] // Skip whitespace but count it for location tracking
#[logos(skip r"//[^\n]*")] // Skip single-line comments
#[logos(skip r"/\*(?:[^*]|\*[^/])*\*/")] // Skip multi-line comments
pub enum Token {
    // Keywords
    #[token("let")]
    Let,

    #[token("const")]
    Const,

    #[token("fn")]
    Fn,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("while")]
    While,

    #[token("for")]
    For,

    #[token("in")]
    In,

    #[token("return")]
    Return,

    #[token("break")]
    Break,

    #[token("continue")]
    Continue,

    #[token("struct")]
    Struct,

    #[token("enum")]
    Enum,

    #[token("match")]
    Match,

    #[token("import")]
    Import,

    #[token("export")]
    Export,

    #[token("as")]
    As,

    #[token("from")]
    From,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("null")]
    Null,

    #[token("undefined")]
    Undefined,

    #[token("async")]
    Async,

    #[token("await")]
    Await,

    #[token("try")]
    Try,

    #[token("catch")]
    Catch,

    #[token("finally")]
    Finally,

    #[token("throw")]
    Throw,

    #[token("typeof")]
    TypeOf,

    #[token("instanceof")]
    InstanceOf,

    // Delimiters
    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token(",")]
    Comma,

    #[token(";")]
    Semicolon,

    #[token(".")]
    Dot,

    #[token(":")]
    Colon,

    #[token("?")]
    Question,

    #[token("=>")]
    Arrow,

    // Operators
    #[token("=")]
    Assign,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("%")]
    Percent,

    #[token("**")]
    Power,

    #[token("++")]
    Increment,

    #[token("--")]
    Decrement,

    #[token("==")]
    Equal,

    #[token("!=")]
    NotEqual,

    #[token("<")]
    Less,

    #[token("<=")]
    LessEqual,

    #[token(">")]
    Greater,

    #[token(">=")]
    GreaterEqual,

    #[token("&&")]
    And,

    #[token("||")]
    Or,

    #[token("!")]
    Not,

    #[token("&")]
    BitAnd,

    #[token("|")]
    BitOr,

    #[token("^")]
    BitXor,

    #[token("~")]
    BitNot,

    #[token("<<")]
    ShiftLeft,

    #[token(">>")]
    ShiftRight,

    #[token(">>>")]
    UnsignedShiftRight,

    #[token("+=")]
    PlusAssign,

    #[token("-=")]
    MinusAssign,

    #[token("*=")]
    StarAssign,

    #[token("/=")]
    SlashAssign,

    #[token("%=")]
    PercentAssign,

    #[token("&=")]
    BitAndAssign,

    #[token("|=")]
    BitOrAssign,

    #[token("^=")]
    BitXorAssign,

    #[token("<<=")]
    ShiftLeftAssign,

    #[token(">>=")]
    ShiftRightAssign,

    #[token("??")]
    NullishCoalescing,

    #[token("?.")]
    OptionalChaining,

    // Literals and identifiers
    #[regex(r#""([^"\\]|\\["\\nrt]|\\u[0-9a-fA-F]{4})*""#, |lex| parse_string_literal(lex.slice()).ok(), priority = 2)]
    StringLiteral(String),

    #[regex(r"'([^'\\]|\\['\\nrt]|\\u[0-9a-fA-F]{4})*'", |lex| parse_char_literal(lex.slice()).ok(), priority = 2)]
    CharLiteral(char),

    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+|0[xX][0-9a-fA-F]+|0[bB][01]+|0[oO][0-7]+|[0-9]+", |lex| parse_number(lex.slice()).ok(), priority = 1)]
    NumberLiteral(NumericLiteral),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| Some(lex.slice().to_string()), priority = 0)]
    Identifier(String),

    // Template literals
    #[regex(
        r#"`([^`\\]|\\[`\\nrt]|\\u[0-9a-fA-F]{4}|\$[^{]|[^$])*`"#,
        |lex| Some(lex.slice()[1..lex.slice().len()-1].to_string()),
        priority = 3
    )]
    TemplateLiteral(String),

    #[regex(
        r#"`([^`\\]|\\[`\\nrt]|\\u[0-9a-fA-F]{4}|\$[^{]|[^$])*\\\$\{"#,
        |lex| Some(lex.slice()[1..lex.slice().len()-3].to_string()),
        priority = 3
    )]
    TemplateHead(String),

    #[regex(
        r#"}([^`\\]|\\[`\\nrt]|\\u[0-9a-fA-F]{4}|\$[^{]|[^$])*\\\$\{"#,
        |lex| Some(lex.slice()[1..lex.slice().len()-3].to_string()),
        priority = 3
    )]
    TemplateMiddle(String),

    #[regex(
        r#"}([^`\\]|\\[`\\nrt]|\\u[0-9a-fA-F]{4}|\$[^{]|[^$])*`"#,
        |lex| Some(lex.slice()[1..lex.slice().len()-1].to_string()),
        priority = 3
    )]
    TemplateTail(String),

    // Handle errors
    Error,
}

impl Token {
    /// Convert a token to its string representation
    pub fn to_string(&self) -> String {
        match self {
            Token::Let => "let".to_string(),
            Token::Const => "const".to_string(),
            Token::Fn => "fn".to_string(),
            Token::If => "if".to_string(),
            Token::Else => "else".to_string(),
            Token::While => "while".to_string(),
            Token::For => "for".to_string(),
            Token::In => "in".to_string(),
            Token::Return => "return".to_string(),
            Token::Break => "break".to_string(),
            Token::Continue => "continue".to_string(),
            Token::Struct => "struct".to_string(),
            Token::Enum => "enum".to_string(),
            Token::Match => "match".to_string(),
            Token::Import => "import".to_string(),
            Token::Export => "export".to_string(),
            Token::As => "as".to_string(),
            Token::From => "from".to_string(),
            Token::True => "true".to_string(),
            Token::False => "false".to_string(),
            Token::Null => "null".to_string(),
            Token::Undefined => "undefined".to_string(),
            Token::Async => "async".to_string(),
            Token::Await => "await".to_string(),
            Token::Try => "try".to_string(),
            Token::Catch => "catch".to_string(),
            Token::Finally => "finally".to_string(),
            Token::Throw => "throw".to_string(),
            Token::TypeOf => "typeof".to_string(),
            Token::InstanceOf => "instanceof".to_string(),
            Token::LBrace => "{".to_string(),
            Token::RBrace => "}".to_string(),
            Token::LParen => "(".to_string(),
            Token::RParen => ")".to_string(),
            Token::LBracket => "[".to_string(),
            Token::RBracket => "]".to_string(),
            Token::Comma => ",".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::Dot => ".".to_string(),
            Token::Colon => ":".to_string(),
            Token::Question => "?".to_string(),
            Token::Arrow => "=>".to_string(),
            Token::Assign => "=".to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Star => "*".to_string(),
            Token::Slash => "/".to_string(),
            Token::Percent => "%".to_string(),
            Token::Power => "**".to_string(),
            Token::Increment => "++".to_string(),
            Token::Decrement => "--".to_string(),
            Token::Equal => "==".to_string(),
            Token::NotEqual => "!=".to_string(),
            Token::Less => "<".to_string(),
            Token::LessEqual => "<=".to_string(),
            Token::Greater => ">".to_string(),
            Token::GreaterEqual => ">=".to_string(),
            Token::And => "&&".to_string(),
            Token::Or => "||".to_string(),
            Token::Not => "!".to_string(),
            Token::BitAnd => "&".to_string(),
            Token::BitOr => "|".to_string(),
            Token::BitXor => "^".to_string(),
            Token::BitNot => "~".to_string(),
            Token::ShiftLeft => "<<".to_string(),
            Token::ShiftRight => ">>".to_string(),
            Token::UnsignedShiftRight => ">>>".to_string(),
            Token::PlusAssign => "+=".to_string(),
            Token::MinusAssign => "-=".to_string(),
            Token::StarAssign => "*=".to_string(),
            Token::SlashAssign => "/=".to_string(),
            Token::PercentAssign => "%=".to_string(),
            Token::BitAndAssign => "&=".to_string(),
            Token::BitOrAssign => "|=".to_string(),
            Token::BitXorAssign => "^=".to_string(),
            Token::ShiftLeftAssign => "<<=".to_string(),
            Token::ShiftRightAssign => ">>=".to_string(),
            Token::NullishCoalescing => "??".to_string(),
            Token::OptionalChaining => "?.".to_string(),
            Token::StringLiteral(s) => format!("\"{}\"", s),
            Token::CharLiteral(c) => format!("'{}'", c),
            Token::NumberLiteral(num) => match num {
                NumericLiteral::Integer(i) => format!("{}", i),
                NumericLiteral::Float(f) => format!("{}", f),
            },
            Token::Identifier(name) => name.clone(),
            Token::TemplateLiteral(s) => format!("`{}`", s),
            Token::TemplateHead(s) => format!("`{}${{", s),
            Token::TemplateMiddle(s) => format!("}}{}${{", s),
            Token::TemplateTail(s) => format!("}}{}`", s),
            Token::Error => "ERROR".to_string(),
        }
    }

    /// Check if the token is a keyword
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            Token::Let
                | Token::Const
                | Token::Fn
                | Token::If
                | Token::Else
                | Token::While
                | Token::For
                | Token::In
                | Token::Return
                | Token::Break
                | Token::Continue
                | Token::Struct
                | Token::Enum
                | Token::Match
                | Token::Import
                | Token::Export
                | Token::As
                | Token::From
                | Token::True
                | Token::False
                | Token::Null
                | Token::Undefined
                | Token::Async
                | Token::Await
                | Token::Try
                | Token::Catch
                | Token::Finally
                | Token::Throw
                | Token::TypeOf
                | Token::InstanceOf
        )
    }

    /// Check if the token is an operator
    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            Token::Plus
                | Token::Minus
                | Token::Star
                | Token::Slash
                | Token::Percent
                | Token::Power
                | Token::Increment
                | Token::Decrement
                | Token::Equal
                | Token::NotEqual
                | Token::Less
                | Token::LessEqual
                | Token::Greater
                | Token::GreaterEqual
                | Token::And
                | Token::Or
                | Token::Not
                | Token::BitAnd
                | Token::BitOr
                | Token::BitXor
                | Token::BitNot
                | Token::ShiftLeft
                | Token::ShiftRight
                | Token::UnsignedShiftRight
                | Token::Assign
                | Token::PlusAssign
                | Token::MinusAssign
                | Token::StarAssign
                | Token::SlashAssign
                | Token::PercentAssign
                | Token::BitAndAssign
                | Token::BitOrAssign
                | Token::BitXorAssign
                | Token::ShiftLeftAssign
                | Token::ShiftRightAssign
                | Token::NullishCoalescing
                | Token::OptionalChaining
        )
    }

    /// Check if the token is a literal
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            Token::StringLiteral(_)
                | Token::CharLiteral(_)
                | Token::NumberLiteral(_)
                | Token::True
                | Token::False
                | Token::Null
                | Token::Undefined
                | Token::TemplateLiteral(_)
                | Token::TemplateHead(_)
                | Token::TemplateMiddle(_)
                | Token::TemplateTail(_)
        )
    }
}

/// Function to convert raw source position to line and column
pub fn get_line_col(source: &str, pos: usize) -> (usize, usize) {
    let mut line = 1;
    let mut last_line_start = 0;

    for (i, c) in source.char_indices() {
        if i >= pos {
            break;
        }

        if c == '\n' {
            line += 1;
            last_line_start = i + 1;
        }
    }

    let column = pos - last_line_start + 1;
    (line, column)
}

/// Lexer for the language that provides tokens with source location information
pub struct Lexer<'source> {
    source: &'source str,
    logos_lexer: logos::Lexer<'source, Token>,
    filename: String,
}

impl<'source> Lexer<'source> {
    /// Create a new lexer for the given source code
    pub fn new(source: &'source str, filename: String) -> Self {
        Self {
            source,
            logos_lexer: Token::lexer(source),
            filename,
        }
    }

    /// Get the next token with source location information
    pub fn next_token(&mut self) -> Option<Result<TokenWithLocation, String>> {
        let token = self.logos_lexer.next()?;
        let span = self.logos_lexer.span();
        let (line, column) = get_line_col(self.source, span.start);

        let location = SourceLocation {
            file: self.filename.clone(),
            line,
            column,
            length: span.end - span.start,
        };

        match token {
            Ok(token) => Some(Ok(TokenWithLocation {
                token,
                span,
                location,
            })),
            Err(_) => {
                let error_text = self.source[span.clone()].to_string();
                Some(Err(format!(
                    "Lexical error at {}:{}: unexpected token '{}'",
                    line, column, error_text
                )))
            }
        }
    }

    /// Lex the entire source code and return a vector of tokens with location information
    pub fn lex_all(&mut self) -> Result<Vec<TokenWithLocation>, String> {
        let mut tokens = Vec::new();

        while let Some(token_result) = self.next_token() {
            match token_result {
                Ok(token) => tokens.push(token),
                Err(e) => return Err(e),
            }
        }

        Ok(tokens)
    }
}

/// Convenience function to tokenize a source string
pub fn lex_source(source: &str, filename: &str) -> Result<Vec<TokenWithLocation>, String> {
    let mut lexer = Lexer::new(source, filename.to_string());
    lexer.lex_all()
}

/// Unit tests for the lexer
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_keywords() {
        let source = "let const fn if else while for in return break continue";
        let tokens = lex_source(source, "test.lang").unwrap();

        assert_eq!(tokens.len(), 11);
        assert_eq!(tokens[0].token, Token::Let);
        assert_eq!(tokens[1].token, Token::Const);
        assert_eq!(tokens[2].token, Token::Fn);
        assert_eq!(tokens[3].token, Token::If);
        assert_eq!(tokens[4].token, Token::Else);
        assert_eq!(tokens[5].token, Token::While);
        assert_eq!(tokens[6].token, Token::For);
        assert_eq!(tokens[7].token, Token::In);
        assert_eq!(tokens[8].token, Token::Return);
        assert_eq!(tokens[9].token, Token::Break);
        assert_eq!(tokens[10].token, Token::Continue);
    }

    #[test]
    fn test_lexer_operators() {
        let source = "+ - * / % ** ++ -- == != < <= > >= && || ! & | ^ ~ << >> >>> += -= *= /= %= &= |= ^= <<= >>= ?? ?.";
        let tokens = lex_source(source, "test.lang").unwrap();

        assert_eq!(tokens.len(), 36);
        assert_eq!(tokens[0].token, Token::Plus);
        assert_eq!(tokens[1].token, Token::Minus);
        // ... and so on for all other operators
    }

    #[test]
    fn test_lexer_literals() {
        let source = r#"42 3.14 0xFF 0b1010 "hello" 'c' true false null `template`"#;
        let tokens = lex_source(source, "test.lang").unwrap();

        assert_eq!(tokens.len(), 9);

        // Check numeric literals
        match &tokens[0].token {
            Token::NumberLiteral(NumericLiteral::Integer(42)) => {}
            _ => panic!("Expected integer 42"),
        }

        match &tokens[1].token {
            Token::NumberLiteral(NumericLiteral::Float(f)) => {
                assert!((f - 3.14).abs() < f64::EPSILON);
            }
            _ => panic!("Expected float 3.14"),
        }

        match &tokens[2].token {
            Token::NumberLiteral(NumericLiteral::Integer(255)) => {}
            _ => panic!("Expected integer 255 (0xFF)"),
        }

        match &tokens[3].token {
            Token::NumberLiteral(NumericLiteral::Integer(10)) => {}
            _ => panic!("Expected integer 10 (0b1010)"),
        }

        // Check string literal
        match &tokens[4].token {
            Token::StringLiteral(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected string literal"),
        }

        // Check char literal
        match &tokens[5].token {
            Token::CharLiteral(c) => assert_eq!(*c, 'c'),
            _ => panic!("Expected char literal"),
        }

        // Check booleans and null
        assert_eq!(tokens[6].token, Token::True);
        assert_eq!(tokens[7].token, Token::False);
        assert_eq!(tokens[8].token, Token::Null);
    }

    #[test]
    fn test_lexer_error() {
        let source = "let x = 42 @";
        let tokens = lex_source(source, "test.lang");

        assert!(tokens.is_err());
        assert_eq!(
            tokens.unwrap_err(),
            "Lexical error at 1:10: unexpected token '@'"
        );
    }
}
