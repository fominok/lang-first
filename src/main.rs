#[no_mangle]
use std::io::{BufRead, BufReader};
use std::fs::File;

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
    RPar,
    Plus,
    Minus,
    Mul,
    Div,
    Lt,
    Gt,
    Eq,
}

struct LexError(String);

#[derive(Debug)]
struct Lexer {
    buf: String,
    tokens: Vec<Token>,
    ignore: bool,
}

impl Lexer {
    fn new() -> Self {
        Lexer {
            buf: String::new(),
            tokens: vec![],
            ignore: false,
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
                    ")" => Some(Token::RPar),
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

                let result = num_token().or(spec_token()).or(name_token());
                if let Some(token) = result {
                    self.tokens.push(token);
                    None
                } else {
                    Some(LexError(p.clone()))
                }
            };
            self.buf.clear();
            r
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

    fn read_char(&mut self, c: char) -> Option<LexError> {
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
                self.tokens.push(Token::Comma);
                r
            } else if c == ';' {
                let r = self.place_pending();
                self.tokens.push(Token::Semicolon);
                r
            } else if let Some(t) = self.read_op(c) {
                self.tokens.push(t);
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
            if let Some(e) = lexer.read_char(c) {
                panic!("Lex error: row {} col {}; buf: {}; Tokens passed: {:?}", i, j, e.0, lexer.tokens);
            }
        }
        lexer.place_pending();
    }
    println!("{:?}", lexer);
}
