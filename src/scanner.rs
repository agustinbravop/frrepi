#[derive(PartialEq)]
pub enum TokenKind {
    // Logical Operators (generally)
    And,
    Or,
    Not,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Arithmetic Operators (generally)
    Plus,
    Minus,
    Star,
    Slash,
    Div,
    Mod,
    // Other special symbols
    Caret,
    Dot,
    DotDot,
    Comma,
    Colon,
    ColonEqual,
    LeftParen,
    RightParen,
    // Keywords
    Mientras,
    Hacer,
    FinMientras,
    Repetir,
    Hasta,
    Que,
    FinRepetir,
    Para,
    FinPara,
    Si,
    Entonces,
    Sino,
    FinSi,
    Funcion,
    Es,
    FinFuncion,
    Procedimiento,
    FinProcedimiento,
    Registro,
    De,
    FinRegistro,
    Fin,
    // Data Types
    False,
    True,
    Nil,
    Integer,
    Decimal,
    String,
    Identifier,
    // These define the structure of the script
    Accion,
    Ambiente,
    Proceso,
    FinAccion,
    // Related to file format
    NewLine,
    Eof,
    Error,
}

pub struct Token {
    kind: TokenKind,
    lexeme: String,
    line: u32,
    column: usize,
}

struct Cursor {
    line: u32,
    line_idx: u32, // line_idx is the idx of first char of current line
    start: usize,  // start is the idx of first char of token to be created
    // column can be computed as (start - line_idx + 1)
    current: usize, // current is the idx of current char
}

struct Scanner<'a> {
    source: &'a str,
    chars: Vec<char>,
    cur: Cursor,
}

impl Scanner<'_> {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source,
            chars: source.chars().collect(),
            cur: Cursor {
                line: 1,
                line_idx: 0,
                start: 0,
                current: 0,
            }, // count from start of line
        }
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        Token {
            kind,
            lexeme: self.source[self.cur.start..self.cur.current].to_string(),
            line: self.cur.line,
            column: self.cur.current,
        }
    }

    fn error_token(&self, msg: String) -> Token {
        Token {
            kind: TokenKind::Error,
            lexeme: msg,
            line: self.cur.line,
            column: self.cur.current,
        }
    }

    fn advance(&mut self) -> Option<&char> {
        self.cur.current += 1;
        self.chars.get(self.cur.current - 1)
    }

    fn matches(&mut self, expected: &char) -> bool {
        if self.peek() == Some(expected) {
            self.cur.current += 1;
            true
        } else {
            false
        }
    }

    fn peek(&self) -> Option<&char> {
        self.chars.get(self.cur.current)
    }

    fn peek_next(&self) -> Option<&char> {
        self.chars.get(self.cur.current + 1)
    }

    fn number(&mut self) -> Token {
        while is_digit(self.peek()) {
            self.advance();
        }

        // Look for a fractional part.
        if self.peek() == Some(&'.') && is_digit(self.peek_next()) {
            // Consume the '.'.
            self.advance();

            while is_digit(self.peek()) {
                self.advance();
            }

            self.make_token(TokenKind::Decimal)
        } else {
            self.make_token(TokenKind::Integer)
        }
    }

    fn string(&mut self) -> Token {
        let mut opt = self.peek();
        while opt != Some(&'"') && opt.is_some() {
            opt = self.advance();
        }

        if self.peek().is_none() {
            self.error_token("String sin comillas de cierre.".to_owned())
        } else {
            // Consume the closing '"'.
            self.advance();
            self.make_token(TokenKind::String)
        }
    }

    // reads the word, then checks if it is a keyword or not
    fn identifier(&mut self) -> Token {
        while is_digit(self.peek()) || is_alpha(self.peek()) {
            self.advance();
        }

        self.make_token(self.get_identifier_type())
    }

    fn get_identifier_type(&self) -> TokenKind {
        let word: String = self.chars.as_slice()[self.cur.start..self.cur.current]
            .iter()
            .collect::<String>()
            .to_uppercase();

        return match word.chars().nth(0).unwrap() {
            'A' => match word.chars().nth(1) {
                Some('C') => check_keyword(&word, "ACCION", TokenKind::Accion),
                Some('M') => check_keyword(&word, "AMBIENTE", TokenKind::Ambiente),
                _ => TokenKind::Identifier,
            },
            'D' => match word.chars().nth(1) {
                Some('E') => check_keyword(&word, "DE", TokenKind::De),
                Some('I') => check_keyword(&word, "DIV", TokenKind::Div),
                _ => TokenKind::Identifier,
            },
            'E' => match word.chars().nth(1) {
                Some('N') => check_keyword(&word, "ENTONCES", TokenKind::Entonces),
                Some('S') => check_keyword(&word, "ES", TokenKind::Es),
                _ => TokenKind::Identifier,
            },
            'F' => match word.chars().nth(1) {
				Some('A') => check_keyword(&word, "FALSO", TokenKind::False),
				Some('U') => check_keyword(&word, "FUNCION", TokenKind::Funcion),
				Some('I') => {
					if let "FIN" = &word[0..2] {
						fin_keyword(&word)	
					} else {
						TokenKind::Identifier
					}
				},
                _ => TokenKind::Identifier,
			}
            'H' => {
                if let Some('A') = word.chars().nth(1) {
                    return match word.chars().nth(2) {
                        Some('C') => check_keyword(&word, "HACER", TokenKind::Hacer),
                        Some('S') => check_keyword(&word, "HASTA", TokenKind::Hasta),
                        _ => TokenKind::Identifier,
                    };
                } else {
                    TokenKind::Identifier
                }
            }
            'M' => match word.chars().nth(1) {
                Some('I') => check_keyword(&word, "MIENTRAS", TokenKind::Mientras),
                Some('O') => check_keyword(&word, "MOD", TokenKind::Mod),
                _ => TokenKind::Identifier,
            },
            'N' => match word.chars().nth(1) {
                Some('I') => check_keyword(&word, "NIL", TokenKind::Nil),
                Some('O') => check_keyword(&word, "NO", TokenKind::Not),
                _ => TokenKind::Identifier,
            },
            'O' => check_keyword(&word, "O", TokenKind::Or),
            'P' => match word.chars().nth(1) {
                Some('A') => check_keyword(&word, "PARA", TokenKind::Mientras),
                Some('R') => {
                    return if word == "PROCEDIMIENTO" {
                        TokenKind::Procedimiento
                    } else if word == "PROCESO" {
                        TokenKind::Proceso
                    } else {
                        TokenKind::Identifier
                    }
                }
                _ => TokenKind::Identifier,
            },
            'Q' => check_keyword(&word, "QUE", TokenKind::Or),
            'R' => {
                if let Some('E') = word.chars().nth(1) {
                    return match word.chars().nth(2) {
                        Some('G') => check_keyword(&word, "REGISTRO", TokenKind::Registro),
                        Some('P') => check_keyword(&word, "REPETIR", TokenKind::Repetir),
                        _ => TokenKind::Identifier,
                    };
                } else {
                    TokenKind::Identifier
                }
            }
            'S' => {
                if let Some('I') = word.chars().nth(1) {
                    return match word.chars().nth(2) {
                        None => check_keyword(&word, "SI", TokenKind::Si),
                        Some('N') => check_keyword(&word, "SINO", TokenKind::Sino),
                        _ => TokenKind::Identifier,
                    };
                } else {
                    TokenKind::Identifier
                }
            }
            'V' => check_keyword(&word, "VERDADERO", TokenKind::True),
            'Y' => check_keyword(&word, "Y", TokenKind::And),

            _ => TokenKind::Identifier,
        };
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        self.cur.start = self.cur.current;

        let opt = self.advance();

        if opt.is_none() {
            return self.make_token(TokenKind::Eof);
        }
        let ch = opt.unwrap();

        if is_digit(opt) {
            return self.number();
        }

        if ch.is_alphabetic() || ch == &'_' {
            return self.identifier();
        }

        return match ch {
            '(' => self.make_token(TokenKind::LeftParen),
            ')' => self.make_token(TokenKind::RightParen),
            '+' => self.make_token(TokenKind::Plus),
            '-' => self.make_token(TokenKind::Minus),
            '*' => self.make_token(TokenKind::Star),
            '/' => self.make_token(TokenKind::Slash),
            '^' => self.make_token(TokenKind::Caret),
            ',' => self.make_token(TokenKind::Comma),
            '=' => self.make_token(TokenKind::Equal),
            '"' => self.string(),

            '.' => {
                let kind = if self.matches(&'.') {
                    TokenKind::DotDot
                } else {
                    TokenKind::Dot
                };
                self.make_token(kind)
            }
            ':' => {
                let kind = if self.matches(&'=') {
                    TokenKind::ColonEqual
                } else {
                    TokenKind::Colon
                };
                self.make_token(kind)
            }
            '<' => {
                let kind = if self.matches(&'=') {
                    TokenKind::LessEqual
                } else if self.matches(&'>') {
                    TokenKind::NotEqual
                } else {
                    TokenKind::Less
                };
                self.make_token(kind)
            }
            '>' => {
                let kind = if self.matches(&'=') {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                };
                self.make_token(kind)
            }

            _ => {
                let msg = format!("Caracter inesperado: {}.", ch);
                self.error_token(msg)
            }
        };
    }

    fn skip_whitespace(&mut self) {
        loop {
            let ch = self.advance();

            match ch {
                Some(' ') => continue,
                Some('\t') => continue,
                Some('\r') => continue,
                Some('\n') => {
                    self.make_token(TokenKind::NewLine);
                    self.cur.line += 1;
                }
                Some('/') => {
                    // One-line comment
                    if let Some('/') = self.peek() {
                        while self.peek().is_some() && self.peek() != Some(&'\n') {
                            self.advance();
                        }
                    }

                    // Multi-line comment
                    if let Some('*') = self.peek() {
                        while self.peek().is_some() {
                            self.advance();

                            if let Some('\n') = self.peek() {
                                self.cur.line += 1;
                            }
                        }
                    }
                }
                _ => return,
            }
        }
    }
}

fn fin_keyword(word: &str) -> TokenKind {
	if word.len() == 3 { return TokenKind::Fin }

	let rest = if &word[3..3] == "_" {
		&word[4..]
	} else {
		&word[3..]
	};

	match &rest[0..0] {
		"A" => check_keyword(rest, "ACCION", TokenKind::FinAccion),
		"F" => check_keyword(rest, "FUNCION", TokenKind::FinFuncion),
		"M" => check_keyword(rest, "MIENTRAS", TokenKind::FinMientras),
        "P" => match &rest[1..1] {
            "A" => check_keyword(rest, "PARA", TokenKind::FinPara),
            "R" => check_keyword(rest, "PROCEDIMIENTO", TokenKind::FinProcedimiento),
            _ => TokenKind::Identifier,
        },
        "R" => match &rest[1..2] {
            "EP" => check_keyword(rest, "REPETIR", TokenKind::FinRepetir),
            "EG" => check_keyword(rest, "REGISTRO", TokenKind::FinRegistro),
            _ => TokenKind::Identifier,
        },
        "S" => check_keyword(rest, "SI", TokenKind::FinSi),
        _ => TokenKind::Identifier
	}
}

fn check_keyword(read: &str, keyword: &str, kind: TokenKind) -> TokenKind {
    if read == keyword {
        kind
    } else {
        TokenKind::Identifier
    }
}

fn is_digit(opt_char: Option<&char>) -> bool {
    if let Some(ch) = opt_char {
        ch.is_ascii_digit()
    } else {
		false
	}
}

fn is_alpha(opt_char: Option<&char>) -> bool {
    opt_char.is_some() && opt_char.unwrap().is_ascii_alphabetic() || opt_char == Some(&'_')
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, TokenKind> {
    let mut tokens = vec![];
    let mut scanner = Scanner::new(input);

    loop {
        let next_token = scanner.scan_token() 
        tokens.push(next_token)

        if next_token.kind == TokenKind::Eof {

        }




    }

    Ok(tokens)
}
