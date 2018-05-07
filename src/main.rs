#[no_mangle]

#[macro_use] extern crate prettytable;
use prettytable::Table;

use std::io::{BufRead, BufReader};
use std::fs::File;
use std::fmt;

#[derive(Debug)]
enum Token {
    Var,
    VarName(String),
    Comma,
    Semicolon,
    Begin,
    End,
    EndDot,
    Assert,
    Number(u32),
    While,
    Do,
    Lpar,
    Rpar,
    Plus,
    Minus,
    Mul,
    Div,
    Lt,
    Gt,
    Eq,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

struct TokenEntry {
    token: Token,
    row: usize,
    col: usize,
}

#[derive(Debug)]
enum TokenClass {
    Number,
    Name,
    Keyword,
    Symbol,
}

impl fmt::Display for TokenClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn classify(t: &Token) -> TokenClass {
    match *t {
        Token::Var | Token::Begin | Token::End
            | Token::EndDot | Token::While | Token::Do => TokenClass::Keyword,
        Token::Comma | Token::Semicolon | Token::Assert
            | Token::Lpar | Token::Rpar | Token::Plus
            | Token::Minus | Token::Mul | Token::Div
            | Token::Lt | Token::Gt | Token::Eq => TokenClass::Symbol,
        Token::VarName(_) => TokenClass::Name,
        Token::Number(_) => TokenClass::Number,
    }
}

struct LexError(String);

struct Lexer {
    buf: String,
    tokens: Vec<TokenEntry>,
    ignore: bool,
    row: usize,
    col: usize,
}

impl Lexer {
    fn new() -> Self {
        Lexer {
            buf: String::new(),
            tokens: vec![],
            ignore: false,
            row: 0,
            col: 0
        }
    }

    fn place_pending(&mut self) -> Option<LexError> {
        if !self.buf.is_empty() {
            let r = {
                let p = &mut self.buf;

                let num_token = || p.parse::<u32>().map(|u| Token::Number(u)).ok();

                let spec_token = || match p.as_ref() {
                    "Var" => Some(Token::Var),
                    "Begin" => Some(Token::Begin),
                    "End" => Some(Token::End),
                    "End." => Some(Token::EndDot),
                    ":=" => Some(Token::Assert),
                    "WHILE" => Some(Token::While),
                    "DO" => Some(Token::Do),
                    "(" => Some(Token::Lpar),
                    ")" => Some(Token::Rpar),
                    "+" => Some(Token::Plus),
                    "-" => Some(Token::Minus),
                    "*" => Some(Token::Mul),
                    "/" => Some(Token::Div),
                    _ => None
                };

                let name_token = || {
                    if p.chars().all(|c| c.is_ascii_alphabetic()) {
                        Some(Token::VarName(p.clone()))
                    } else {
                        None
                    }
                };

                num_token().or(spec_token()).or(name_token())
            };
            let result = if let Some(token) = r {
                self.push(token);
                None
            } else {
                Some(LexError(self.buf.clone()))
            };
            self.buf.clear();
            result
        } else {
            None
        }
    }

    fn read_op(&mut self, c: char) -> Option<Token> {
        if self.buf == ":" && c == '=' {
            self.buf.clear();
            Some(Token::Assert)
        } else {
            match c {
                '*' => Some(Token::Mul),
                '/' => Some(Token::Div),
                '+' => Some(Token::Plus),
                '-' => Some(Token::Minus),
                '<' => Some(Token::Lt),
                '>' => Some(Token::Gt),
                '=' => Some(Token::Eq),
                _ => None
            }
        }
    }

    fn push(&mut self, t: Token) {
        let te = TokenEntry { token: t, row: self.row, col: self.col };
        self.tokens.push(te);
    }

    fn read_char(&mut self, row: usize, col: usize, c: char) -> Option<LexError> {
        self.row = row;
        self.col = col;

        if self.ignore {
            if c == '}' {
                self.ignore = false;
            }
            None
        } else {
            if c == '{' {
                self.ignore = true;
                None
            } else if c == '}' {
                Some(LexError("}".to_string()))
            } else if c.is_whitespace() || c == '\r' {
                self.place_pending()
            } else if c == ',' {
                let r = self.place_pending();
                self.push(Token::Comma);
                r
            } else if c == ';' {
                let r = self.place_pending();
                self.push(Token::Semicolon);
                r
            } else if let Some(t) = self.read_op(c) {
                self.push(t);
                None
            } else {
                self.buf.push(c);
                None
            }
        }
    }
}

fn main() {
    let file = File::open("input").expect("Cannot open file");
    let reader = BufReader::new(file);

    let mut lexer = Lexer::new();

    for (i, line) in reader.lines().enumerate() {
        let l = line.expect("File read error");
        for (j, c) in l.chars().enumerate() {
            if let Some(e) = lexer.read_char(i, j, c) {
                panic!("Lex error: row {} col {}; buf: {};", i, j, e.0);
            }
        }
        lexer.place_pending();
    }

    let mut table = Table::new();

    table.add_row(row!["Position", "Token", "Class"]);
    for token in lexer.tokens {
        let pos = format!("Row: {}, Col: {}", token.row + 1, token.col);
        let t = token.token;
        let row = row![pos, t, classify(&t)];
        table.add_row(row);
    }

    table.printstd();
}
