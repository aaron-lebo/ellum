use serde::{Deserialize, Serialize};
use std::io::Read;

#[derive(Clone, Debug)]
enum TokenK {
    Comment,
    Number,
    String,
    Symbol,
    Other,
}

#[derive(Clone, Debug)]
struct Token {
    kind: TokenK,
    i: usize,
    n: usize,
    row: u64,
    col: u64,
}

impl Token {
    fn str<'a>(&self, s: &'a str) -> &'a str {
        s.get(self.i..self.n + self.i).unwrap()
    }

    fn is_symbol(&self) -> bool {
        matches!(self.kind, TokenK::Symbol)
    }
}

#[derive(Debug)]
struct Tokenizer<'a> {
    str: &'a str,
    chars: std::str::Chars<'a>,
    i: usize,
    ch: Option<char>,
    ch1: Option<char>,
    row: u64,
    col: u64,
    mark: (usize, u64, u64),
    err: Option<String>,
}

impl Iterator for Tokenizer<'_> {
    type Item = Result<Token, String>;

    fn next(&mut self) -> Option<Self::Item> {
        (self.ch.is_some() && self.err.is_none()).then(|| {
            while self.step_if(|x, _| x == '\n' || x == ' ') {}
            self.get_token().map_err(|x| {
                self.err = Some(x.clone());
                x
            })
        })
    }
}

impl Tokenizer<'_> {
    fn get_token(&mut self) -> Result<Token, String> {
        self.mark = (self.i, self.row, self.col);
        if self.step2('-', '-') {
            // .or_else
            while self.step_if(|x, _| x != '\n') {}
            self.token(TokenK::Comment)
        } else if self.step2('-', '>') || self.step2('.', '.') {
            self.token(TokenK::Other)
        } else if self.step_if(|x, _| x == '\"') {
            while self.step2('\\', '\"') || self.step_if(|x, _| x != '\"') {}
            self.step();
            self.token(TokenK::String)
        } else if self.step_if(|x, _| x.is_numeric()) {
            let mut dot = false;
            while self.step_if(|x, _| !dot && x == '.' || x.is_numeric()) {
                dot = dot || self.ch == Some('.');
            }
            self.token(TokenK::Number)
        } else if self.step_if(|x, _| x.is_alphabetic() || x == '_') {
            while self.step_if(|x, y| x == '.' && y != Some('.') || x.is_alphanumeric() || x == '_')
            {
            }
            self.token(TokenK::Symbol)
        } else {
            for c in "()*+,-/:=[]{|}".chars() {
                if self.step_if(|x, _| c == x) {
                    return self.token(TokenK::Other);
                }
            }
            Err("invalid token".to_string())
        }
    }

    fn step2(&mut self, c: char, c1: char) -> bool {
        if self.ch == Some(c) && self.ch1 == Some(c1) {
            self.step();
            self.step();
            return true;
        }
        false
    }

    fn step_if<F>(&mut self, f: F) -> bool
    where
        F: Fn(char, Option<char>) -> bool,
    {
        if self.ch.is_some_and(|x| f(x, self.ch1)) {
            self.step();
            return true;
        }
        false
    }

    fn step(&mut self) {
        self.i += 1;
        if self.ch1 == Some('\n') {
            self.row += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }

        self.ch = self.ch1;
        self.ch1 = self.chars.next();
    }

    fn token(&self, kind: TokenK) -> Result<Token, String> {
        Ok(Token {
            kind,
            i: self.mark.0,
            n: self.i - self.mark.0,
            row: self.mark.1,
            col: self.mark.2,
        })
    }
}

#[derive(Clone, Debug, Serialize)]
struct Pos {
    i: usize,
    n: usize,
    row: u64,
    col: u64,
}

impl From<Token> for Pos {
    fn from(t: Token) -> Self {
        Pos {
            i: t.i,
            n: t.n,
            row: t.row,
            col: t.col,
        }
    }
}

#[derive(Clone, Debug, Serialize)]
enum Node {
    Bind {
        pos: Pos,
        bindings: Vec<Node>,
        exp: Box<Node>,
    },
    Call {
        f: Box<Node>,
        args: Vec<Node>,
    },
    Case {
        pos: Pos,
        exp: Box<Node>,
        arms: Vec<Node>,
    },
    Comment(Pos),
    Import {
        pos: Pos,
        expose: Option<Box<Node>>,
    },
    Infix(Pos, Box<Node>, Box<Node>),
    Number(Pos),
    String(Pos),
    Struct(Pos, Vec<Node>),
    Symbol(Pos),
    Tuple(Pos, Vec<Node>),
    Type {
        pos: Pos,
        alias: bool,
        symbol: Box<Node>,
        exp: Box<Node>,
    },
    Variant(Vec<Node>),
    Vector(Pos, Vec<Node>),
}

#[derive(Debug)]
struct Parser<'a> {
    tokenizer: Tokenizer<'a>,
    tk: Option<Token>,
    tk1: Option<Token>,
}

impl Iterator for Parser<'_> {
    type Item = Node;

    fn next(&mut self) -> Option<Self::Item> {
        if self.tk.is_none() {
            return None;
        }
        match self.parse(0) {
            Ok(x) => {
                self.step();
                Some(x)
            }
            Err(x) => {
                dbg!(x);
                None
            }
        }
    }
}

impl Parser<'_> {
    fn parse(&mut self, power: u8) -> Result<Node, String> {
        let t = self.tk.as_ref().unwrap();
        let prefix = match t.kind {
            TokenK::Comment => Self::comment,
            TokenK::Number => Self::number,
            TokenK::String => Self::string,
            TokenK::Symbol => match self.str(&t) {
                "case" => Self::case,
                "import" => Self::import,
                "let" => Self::let_,
                "type" => Self::type_,
                _ => Self::call,
            },
            TokenK::Other => match self.str(&t) {
                "(" => Self::tuple,
                "[" => Self::vector,
                "{" => Self::struct_,
                _ => Self::error,
            },
            _ => Self::error,
        };
        prefix(self).and_then(|mut node| {
            let mut p = self.power();
            while power < p {
                match self.step().infix(node, p) {
                    Ok(x) => node = x,
                    err => return err,
                }
                p = self.power();
            }
            Ok(node)
        })
    }

    fn error(&mut self) -> Result<Node, String> {
        Err(format!("missing parser: {:?}", self.tk))
    }

    fn infix(&mut self, left: Node, power: u8) -> Result<Node, String> {
        let t = self.tk.take().unwrap();
        let p = match self.str(&t) {
            "->" | ":" | "=" | "|" => power - 1,
            _ => power,
        };
        Ok(Node::Infix(
            t.into(),
            Box::new(left),
            Box::new(self.step().parse(p)?),
        ))
    }

    fn call(&mut self) -> Result<Node, String> {
        let t = self.tk.take().unwrap();
        let mut args = vec![];
        while self.power() == 0
            && self.tk1.as_ref().is_some_and(|x| {
                t.col < x.col
                    && self.str(&x) != "of"
                    && !self.str(&x).contains(&[')', ',', ']', '}'])
            })
        {
            args.push(if let Some(s) = self.if_(|x, _| x.is_symbol()) {
                Node::Symbol(s.into())
            } else {
                self.step().parse(0)?
            });
        }

        let s = Node::Symbol(t.into());
        if args.is_empty() {
            return Ok(s);
        }
        Ok(Node::Call {
            f: Box::new(s),
            args,
        })
    }

    fn power(&self) -> u8 {
        self.tk1.as_ref().map_or(0, |x| match self.str(&x) {
            "->" | ":" | "=" | "|" => 4,
            "*" => 6,
            "/" => 6,
            "+" => 5,
            "-" => 5,
            _ => 0,
        })
    }

    fn case(&mut self) -> Result<Node, String> {
        let t = self.tk.take().unwrap();
        let exp = self.step().parse(0)?;
        let mut arms = vec![];
        self.if_or(|x, s| x.str(s) == "of", "expected 'of'")?;
        while self.power() == 0 && self.tk1.as_ref().is_some_and(|x| t.col < x.col) {
            arms.push(self.step().parse(0)?);
        }
        Ok(Node::Case {
            pos: t.into(),
            exp: Box::new(exp),
            arms,
        })
    }

    fn comment(&mut self) -> Result<Node, String> {
        Ok(Node::Comment(self.tk.take().unwrap().into()))
    }

    fn import(&mut self) -> Result<Node, String> {
        let module = self.if_or(|x, _| x.is_symbol(), "expected symbol")?;
        let mut expose = None;
        if self.if_(|x, s| x.str(s) == "exposing").is_some() {
            self.if_or(|x, s| x.str(s) == "(", "expected (")?;
            expose = Some(self.if_or(
                |x, s| x.is_symbol() || x.str(s) == "..",
                "expected .. or symbol",
            )?);
            self.if_or(|x, s| x.str(s) == ")", "expected )")?;
        }
        Ok(Node::Import {
            pos: module.into(),
            expose: expose.map(|x| Box::new(Node::Symbol(x.into()))),
        })
    }

    fn let_(&mut self) -> Result<Node, String> {
        let t = self.tk.take().unwrap();
        let mut bindings = vec![];
        while self.tk1.as_ref().is_some_and(|x| self.str(&x) != "in") {
            bindings.push(self.step().parse(0)?);
        }
        self.if_or(|x, s| x.str(s) == "in", "expected 'in'")?;
        Ok(Node::Bind {
            pos: t.into(),
            bindings,
            exp: Box::new(self.step().parse(0)?),
        })
    }

    fn number(&mut self) -> Result<Node, String> {
        Ok(Node::Number(self.tk.take().unwrap().into()))
    }

    fn string(&mut self) -> Result<Node, String> {
        Ok(Node::String(self.tk.take().unwrap().into()))
    }

    fn struct_(&mut self) -> Result<Node, String> {
        let (pos, nodes) = self.group("}")?;
        Ok(Node::Struct(pos, nodes))
    }

    fn tuple(&mut self) -> Result<Node, String> {
        let (pos, nodes) = self.group(")")?;
        Ok(Node::Tuple(pos, nodes))
    }

    fn vector(&mut self) -> Result<Node, String> {
        let (pos, nodes) = self.group("]")?;
        Ok(Node::Vector(pos, nodes))
    }

    fn group(&mut self, end: &str) -> Result<(Pos, Vec<Node>), String> {
        let t = self.tk.take().unwrap();
        let mut nodes = vec![];
        while self.tk1.as_ref().is_some_and(|x| self.str(&x) != end) {
            nodes.push(self.step().parse(0)?);
            self.if_(|x, s| x.str(s) == ",");
        }
        self.step();
        Ok((t.into(), nodes))
    }

    fn type_(&mut self) -> Result<Node, String> {
        let t = self.tk.take().unwrap();
        let mut s = self.if_or(|x, _| x.is_symbol(), "expected symbol")?;
        let alias = self.str(&s) == "alias";
        if alias {
            s = self.if_or(|x, _| x.is_symbol(), "expected symbol")?;
        }

        self.if_or(|x, s| x.str(s) == "=", "expected =")?;

        let mut variants = vec![];
        while self
            .tk1
            .as_ref()
            .is_some_and(|x| t.col < x.col && x.is_symbol())
        {
            variants.push(self.step().parse(4)?);
            self.if_(|x, s| x.str(s) == "|");
        }

        Ok(Node::Type {
            pos: t.into(),
            alias,
            symbol: Box::new(Node::Symbol(s.into())),
            exp: Box::new(if variants.is_empty() {
                self.step().parse(0)?
            } else {
                Node::Variant(variants)
            }),
        })
    }

    fn if_or<F>(&mut self, f: F, err: &str) -> Result<Token, String>
    where
        F: FnOnce(&Token, &str) -> bool,
    {
        self.if_(f).ok_or(err.to_string())
    }

    fn if_<F>(&mut self, f: F) -> Option<Token>
    where
        F: FnOnce(&Token, &str) -> bool,
    {
        f(self.tk1.as_ref()?, self.tokenizer.str).then(|| {
            let x = self.tk1.take();
            self.step();
            x.unwrap()
        })
    }

    fn step(&mut self) -> &mut Self {
        self.tk = self.tk1.take();
        self.tk1 = self.tokenizer.next().and_then(|x| x.ok());
        self
    }

    fn str<'a>(&'a self, t: &Token) -> &'a str {
        t.str(self.tokenizer.str)
    }
}

pub fn main() -> std::io::Result<()> {
    let mut f = std::fs::File::open("clock.elm")?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;

    let mut t = Tokenizer {
        str: &s,
        chars: s.chars(),
        i: 0,
        ch: None,
        ch1: None,
        row: 1,
        col: 1,
        mark: (0, 0, 0),
        err: None,
    };
    t.ch = t.chars.next();
    t.ch1 = t.chars.next();

    let mut p = Parser {
        tokenizer: t,
        tk: None,
        tk1: None,
    };
    p.step();
    p.step();
    println!("{}", serde_json::to_string(&p.collect::<Vec<_>>())?);
    Ok(())
}
