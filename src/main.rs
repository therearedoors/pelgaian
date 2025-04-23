use clap::{Parser as ClapParser, Subcommand};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

// CLI argument parsing
#[derive(ClapParser)]
#[command(name = "pelgaian")]
#[command(about = "Pelgaian programming language interpreter", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Run {
        #[arg(value_name = "FILE")]
        file: String,
    },
}

// Token types for the lexer
#[derive(Debug, PartialEq, Clone)]
enum Token {
    Fun, Pro, Let, Ident(String), Number(f64), Global,
    LParen, RParen, LBrace, RBrace, Comma, Semicolon, Dot,
    Plus, Minus, Star, Slash, Equals,
    EOF,
}

// Lexer to tokenize input code
struct Lexer {
    input: Vec<char>,
    position: usize,
}

impl Lexer {
    fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
            position: 0,
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        if self.position >= self.input.len() {
            return Token::EOF;
        }
        let ch = self.input[self.position];
        self.position += 1;
        match ch {
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '.' => Token::Dot,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::Slash,
            '=' => Token::Equals,
            _ if ch.is_alphabetic() => {
                let mut ident = ch.to_string();
                while self.position < self.input.len() && (self.input[self.position].is_alphanumeric() || self.input[self.position] == '_') {
                    ident.push(self.input[self.position]);
                    self.position += 1;
                }
                match ident.as_str() {
                    "fun" => Token::Fun,
                    "pro" => Token::Pro,
                    "let" => Token::Let,
                    "global" => Token::Global,
                    _ => Token::Ident(ident),
                }
            }
            _ if ch.is_digit(10) || ch == '.' => {
                let mut num = ch.to_string();
                while self.position < self.input.len() && (self.input[self.position].is_digit(10) || self.input[self.position] == '.') {
                    num.push(self.input[self.position]);
                    self.position += 1;
                }
                Token::Number(num.parse().unwrap_or(0.0))
            }
            _ => Token::EOF,
        }
    }

    fn skip_whitespace(&mut self) {
        while self.position < self.input.len() && self.input[self.position].is_whitespace() {
            self.position += 1;
        }
    }
}

// AST nodes
#[derive(Debug, Clone)]
enum Expr {
    Number(f64),
    Ident(String),
    BinaryOp(Box<Expr>, Token, Box<Expr>),
    Call(String, Vec<Expr>),
    GlobalAccess(String),
}

#[derive(Debug, Clone)]
enum Stmt {
    Let(String, Expr),
    Fun(String, Vec<String>, Expr), // name, params, body, return expr
    Pro(String, Vec<String>, Vec<Stmt>),      // name, params, body
    Expr(Expr),
}

// Parser to build AST
struct PelgaianParser {
    tokens: Vec<Token>,
    position: usize,
}

impl PelgaianParser {
    fn new(tokens: Vec<Token>) -> Self {
        PelgaianParser { tokens, position: 0 }
    }

    fn current(&self) -> &Token {
        self.tokens.get(self.position).unwrap_or(&Token::EOF)
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while self.current() != &Token::EOF {
            println!("Parsing statement at position {}: {:?}", self.position, self.current());
            stmts.push(self.parse_stmt());
        }
        stmts
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.current() {
            Token::Let => {
                self.advance();
                let name = if let Token::Ident(name) = self.current() {
                    let name = name.clone();
                    self.advance();
                    name
                } else {
                    panic!("Expected identifier after let at position {}", self.position);
                };
                if self.current() == &Token::Equals {
                    self.advance();
                } else {
                    panic!("Expected = after identifier at position {}", self.position);
                }
                let expr = self.parse_expr();
                if self.current() == &Token::Semicolon {
                    self.advance();
                } else {
                    panic!("Expected ; after let statement at position {}", self.position);
                }
                Stmt::Let(name, expr)
            }
            Token::Fun => {
                self.advance();
                let name = if let Token::Ident(name) = self.current() {
                    let name = name.clone();
                    self.advance();
                    name
                } else {
                    panic!("Expected identifier after fun at position {}", self.position);
                };
                let params = self.parse_params();
                if self.current() != &Token::LBrace {
                    panic!("Expected {{ at position {}", self.position);
                }
                self.advance();
                let body = self.parse_expr();
                if self.current() == &Token::Semicolon {
                    self.advance();
                } else {
                    panic!("Expected ; after fun body expression at position {}", self.position);
                }
                if self.current() != &Token::RBrace {
                    panic!("Expected }} after fun body at position {}", self.position);
                }
                self.advance();
                if self.current() == &Token::Semicolon {
                    self.advance();
                } else {
                    panic!("Expected ; after fun definition at position {}", self.position);
                }
                Stmt::Fun(name, params, body)
            }
            Token::Pro => {
                self.advance();
                let name = if let Token::Ident(name) = self.current() {
                    let name = name.clone();
                    self.advance();
                    name
                } else {
                    panic!("Expected identifier after pro at position {}", self.position);
                };
                let params = self.parse_params();
                let body = self.parse_body();
                if self.current() == &Token::Semicolon {
                    self.advance();
                } else {
                    panic!("Expected ; after pro definition at position {}", self.position);
                }
                Stmt::Pro(name, params, body)
            }
            _ => {
                let expr = self.parse_expr();
                if self.current() == &Token::Semicolon {
                    self.advance();
                } else {
                    panic!("Expected ; after expression at position {}", self.position);
                }
                Stmt::Expr(expr)
            }
        }
    }

    fn parse_params(&mut self) -> Vec<String> {
        let mut params = Vec::new();
        if self.current() != &Token::LParen {
            panic!("Expected ( at position {}", self.position);
        }
        self.advance();
        while self.current() != &Token::RParen {
            if let Token::Ident(name) = self.current() {
                params.push(name.clone());
                self.advance();
                if self.current() == &Token::Comma {
                    self.advance();
                }
            } else {
                panic!("Expected identifier or ) at position {}", self.position);
            }
        }
        self.advance();
        params
    }

    fn parse_body(&mut self) -> Vec<Stmt> {
        let mut body = Vec::new();
        if self.current() != &Token::LBrace {
            panic!("Expected {{ at position {}", self.position);
        }
        self.advance();
        while self.current() != &Token::RBrace {
            body.push(self.parse_stmt());
        }
        self.advance();
        body
    }

    fn parse_expr(&mut self) -> Expr {
        println!("Parsing expr at position {}: {:?}", self.position, self.current());
        let mut expr = self.parse_term();
        while matches!(self.current(), Token::Plus | Token::Minus) {
            let op = self.current().clone();
            self.advance();
            let right = self.parse_term();
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn parse_term(&mut self) -> Expr {
        let mut expr = self.parse_factor();
        while matches!(self.current(), Token::Star | Token::Slash) {
            let op = self.current().clone();
            self.advance();
            let right = self.parse_factor();
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn parse_factor(&mut self) -> Expr {
        println!("Parsing factor at position {}: {:?}", self.position, self.current());
        match self.current() {
            Token::Number(n) => {
                let expr = Expr::Number(*n);
                self.advance();
                expr
            }
            Token::Ident(name) => {
                let name = name.clone();
                self.advance();
                if self.current() == &Token::LParen {
                    self.advance();
                    let mut args = Vec::new();
                    while self.current() != &Token::RParen {
                        args.push(self.parse_expr());
                        if self.current() == &Token::Comma {
                            self.advance();
                        }
                    }
                    self.advance();
                    Expr::Call(name, args)
                } else {
                    Expr::Ident(name)
                }
            }
            Token::Global => {
                self.advance();
                if self.current() != &Token::Dot {
                    panic!("Expected . after global at position {}", self.position);
                }
                self.advance();
                if let Token::Ident(name) = self.current() {
                    let name = name.clone();
                    self.advance();
                    Expr::GlobalAccess(name)
                } else {
                    panic!("Expected identifier after global. at position {}", self.position);
                }
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expr();
                if self.current() != &Token::RParen {
                    panic!("Expected ) at position {}", self.position);
                }
                self.advance();
                expr
            }
            _ => panic!("Unexpected token: {:?} at position {}", self.current(), self.position),
        }
    }
}

// Environment for variable and function/procedure storage
#[derive(Clone)]
struct Env {
    vars: HashMap<String, f64>,
    funs: HashMap<String, (Vec<String>, Vec<Stmt>, Expr)>,
    pros: HashMap<String, (Vec<String>, Vec<Stmt>)>,
    parent: Option<Box<Env>>,
}

impl Env {
    fn new() -> Self {
        Env {
            vars: HashMap::new(),
            funs: HashMap::new(),
            pros: HashMap::new(),
            parent: None,
        }
    }

    fn with_parent(parent: Env) -> Self {
        Env {
            vars: HashMap::new(),
            funs: HashMap::new(),
            pros: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    fn get_var(&self, name: &str) -> Option<f64> {
        self.vars.get(name).copied().or_else(|| {
            self.parent.as_ref().and_then(|p| p.get_var(name))
        })
    }

    fn set_var(&mut self, name: String, value: f64) {
        self.vars.insert(name, value);
    }
}

// Interpreter
struct Interpreter {
    global_env: Env,
    global_state: HashMap<String, f64>,
    in_function: bool,
}

impl Interpreter {
    fn new() -> Self {
        Interpreter {
            global_env: Env::new(),
            global_state: HashMap::new(),
            in_function: false,
        }
    }

    fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<f64, String> {
        let mut last = 0.0;
        let mut env = self.global_env.clone();
        for stmt in stmts {
            last = self.eval_stmt(&stmt, &mut env)?;
        }
        self.global_env = env;
        Ok(last)
    }

    fn eval_stmt(&mut self, stmt: &Stmt, env: &mut Env) -> Result<f64, String> {
        match stmt {
            Stmt::Let(name, expr) => {
                let value = self.eval_expr(expr, env)?;
                env.set_var(name.clone(), value);
                Ok(value)
            }
            Stmt::Fun(name, params, body) => {
                env.funs.insert(name.clone(), (params.clone(), vec![], body.clone()));
                Ok(0.0)
            }
            Stmt::Pro(name, params, body) => {
                env.pros.insert(name.clone(), (params.clone(), body.clone()));
                Ok(0.0)
            }
            Stmt::Expr(expr) => self.eval_expr(expr, env),
        }
    }

    fn eval_expr(&mut self, expr: &Expr, env: &mut Env) -> Result<f64, String> {
        match expr {
            Expr::Number(n) => Ok(*n),
            Expr::Ident(name) => env.get_var(name).ok_or_else(|| format!("Undefined variable: {}", name)),
            Expr::BinaryOp(left, op, right) => {
                let l = self.eval_expr(left, env)?;
                let r = self.eval_expr(right, env)?;
                match op {
                    Token::Plus => Ok(l + r),
                    Token::Minus => Ok(l - r),
                    Token::Star => Ok(l * r),
                    Token::Slash => Ok(l / r),
                    _ => Err("Invalid operator".to_string()),
                }
            }
            Expr::Call(name, args) => {
                let args: Result<Vec<f64>, String> = args.iter().map(|arg| self.eval_expr(arg, env)).collect();
                let args = args?;
                if let Some((params, body, ret)) = env.funs.get(name) {
                    if params.len() != args.len() {
                        return Err("Argument count mismatch".to_string());
                    }
                    let old_in_function = self.in_function;
                    self.in_function = true;
                    let mut local_env = Env::with_parent(env.clone());
                    for (param, arg) in params.iter().zip(args) {
                        local_env.set_var(param.clone(), arg);
                    }
                    for stmt in body {
                        self.eval_stmt(stmt, &mut local_env)?;
                    }
                    let result = self.eval_expr(ret, &mut local_env)?;
                    self.in_function = old_in_function;
                    Ok(result)
                } else if let Some((params, body)) = env.pros.get(name) {
                    if params.len() != args.len() {
                        return Err("Argument count mismatch".to_string());
                    }
                    let mut local_env = Env::with_parent(env.clone());
                    for (param, arg) in params.iter().zip(args) {
                        local_env.set_var(param.clone(), arg);
                    }
                    let mut last = 0.0;
                    for stmt in body {
                        last = self.eval_stmt(stmt, &mut local_env)?;
                    }
                    for (name, value) in local_env.vars.iter() {
                        if name.starts_with("global_") {
                            self.global_state.insert(name.replace("global_", ""), *value);
                        }
                    }
                    Ok(last)
                } else {
                    Err(format!("Undefined function or procedure: {}", name))
                }
            }
            Expr::GlobalAccess(name) => {
                if self.in_function {
                    Err("Global state access not allowed in fun".to_string())
                } else {
                    Ok(*self.global_state.get(name).unwrap_or(&0.0))
                }
            }
        }
    }
}

fn run_file(file_path: &str) -> Result<(), String> {
    let code = fs::read_to_string(file_path)
        .map_err(|e| format!("Failed to read file {}: {}", file_path, e))?;

    let mut lexer = Lexer::new(&code);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token();
        tokens.push(token.clone());
        if token == Token::EOF {
            break;
        }
    }

    let mut parser = PelgaianParser::new(tokens);
    let ast = parser.parse();

    let mut interpreter = Interpreter::new();
    match interpreter.interpret(ast) {
        Ok(result) => {
            println!("Result: {}", result);
            println!("Global state: {:?}", interpreter.global_state);
            Ok(())
        }
        Err(e) => Err(format!("Error: {}", e)),
    }
}

fn main() {
    let cli = Cli::parse();
    match &cli.command {
        Commands::Run { file } => {
            if !file.ends_with(".pg") {
                eprintln!("Error: File must have .pg extension");
                std::process::exit(1);
            }
            if !Path::new(&file).exists() {
                eprintln!("Error: File {} does not exist", file);
                std::process::exit(1);
            }
            if let Err(e) = run_file(&file) {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }
    }
}