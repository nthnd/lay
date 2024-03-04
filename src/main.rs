use pest::Parser;
use pest_derive::Parser;
use rustyline::config::Configurer;
use std::{collections::HashMap, fmt::Display, rc::Rc};

use anyhow::{bail, Context, Result};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct Grammar;

fn read(input: &str) -> Result<Expr> {
    let mut ast = Grammar::parse(Rule::input, input)?;

    if std::env::var("DEBUG_LAY").is_ok() {
        pest_ascii_tree::print_ascii_tree(Ok(ast.clone()));
    }

    #[rustfmt::skip]
    fn parse_value(expr: pest::iterators::Pair<'_, Rule>) -> Expr {
        match expr.as_rule() {
            Rule::nil => Expr::Nil,
            Rule::bool => Expr::Bool(expr.as_str().parse().unwrap()),
            Rule::number => Expr::Number(expr.as_str().parse().unwrap()),
            Rule::symbol => Expr::Symbol(expr.as_str().to_string()),
            Rule::string => Expr::String(expr.as_str().to_string()),
            Rule::list => Expr::List(expr.into_inner().map(parse_value).collect()),
            Rule::EOI
            | Rule::WHITESPACE
            | Rule::inner
            | Rule::symbol_chars
            | Rule::input
            | Rule::char
            | Rule::expr
            | Rule::COMMENT => 
            unreachable!("{expr:?}"),
        }
    }

    let expr = ast.next().context("failed to parse input")?;
    let value = parse_value(expr);

    Ok(value)
}

#[derive(Debug, Clone)]
pub enum Expr {
    Nil,
    Bool(bool),
    Number(i32),
    Symbol(String),
    String(String),
    Lambda { args: Vec<String>, body: Rc<Expr> },
    List(Vec<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Nil => write!(f, "nil"),
            Expr::Bool(x) => write!(f, "{x}"),
            Expr::Number(x) => write!(f, "{x}"),
            Expr::Symbol(x) => write!(f, "{x}"),
            Expr::String(x) => write!(f, "{x}"),
            l @ Expr::Lambda { .. } => write!(f, "{l:#?}"),
            Expr::List(l) => write!(
                f,
                "({})",
                l.iter()
                    .map(|item| format!("{item}"))
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Environment {
    bindings: HashMap<String, Rc<Expr>>,
    parent: Option<Rc<Environment>>,
}

impl Environment {
    fn get(&self, symbol: &str) -> Option<Rc<Expr>> {
        match self.bindings.get(symbol) {
            value @ Some(_) => value.cloned(),
            None => match &self.parent {
                Some(parent) => parent.get(symbol),
                None => None,
            },
        }
    }

    pub fn lookup(&self, symbol: String) -> Result<Expr> {
        match self.get(&symbol) {
            Some(expr) => Ok(expr.as_ref().clone()),
            None => bail!("failed to lookup {symbol:?}"),
        }
    }

    pub fn set(&mut self, symbol: String, value: Expr) -> Option<Rc<Expr>> {
        self.bindings.insert(symbol, Rc::new(value))
    }

    pub fn new_child(&self) -> Self {
        Self {
            parent: Some(Rc::new(self.clone())),
            ..Default::default()
        }
    }
}

fn eval(expr: Expr, env: &mut Environment) -> Result<Expr> {
    match expr {
        Expr::Nil | Expr::Bool(_) | Expr::Number(_) | Expr::String(_) | Expr::Lambda { .. } => {
            Ok(expr)
        }
        Expr::Symbol(s) => env.lookup(s),
        Expr::List(list) => match &list[..] {
            [] => Ok(Expr::Nil),

            // if
            [Expr::Symbol(op_symbol), condition, consequent, alternate] if op_symbol == "if" => {
                match eval(condition.clone(), env)? {
                    Expr::Bool(true) => eval(consequent.clone(), env),
                    Expr::Bool(false) => eval(alternate.clone(), env),
                    x => bail!("expected a bool but found {x:?}"),
                }
            }

            // def
            [Expr::Symbol(op), Expr::Symbol(s), value] if op == "def" => {
                let value = eval(value.clone(), env)?;
                env.set(s.to_string(), value);
                Ok(Expr::Nil)
            }

            // fn
            [Expr::Symbol(op), Expr::List(args), body] if op == "fn" => args
                .into_iter()
                .map(|arg| {
                    if let Expr::Symbol(s) = arg {
                        Ok(s.to_owned())
                    } else {
                        bail!("malformed arg in fn {list:?}")
                    }
                })
                .collect::<Result<Vec<String>>>()
                .map(|args| Expr::Lambda {
                    args,
                    body: Rc::new(body.clone()),
                }),

            // sexp
            [op, rest @ ..] => {
                let op = eval(op.clone(), env)?;
                apply(&op, &rest, env)
            }
        },
    }
}

fn apply(operator: &Expr, rest: &[Expr], env: &mut Environment) -> Result<Expr> {
    match operator {
        Expr::Lambda { args, body } => {
            let mut new_env = env.new_child();
            for (i, arg) in args.into_iter().enumerate() {
                new_env.set(arg.to_owned(), eval(rest[i].clone(), env)?);
            }
            eval(body.as_ref().clone(), &mut new_env)
        }
        _ => unreachable!("i don't know {operator:#?} {rest:#?} {env:#?}"),
    }
}

fn main() {
    let mut rl = rustyline::DefaultEditor::new().unwrap();
    rl.set_edit_mode(rustyline::EditMode::Vi);

    let mut env = Environment::default();

    loop {
        let line = rl.readline("> ").unwrap();

        let expr = match read(&line) {
            Ok(expr) => expr,
            Err(e) => {
                eprintln!("{e}");
                continue;
            }
        };

        let value = eval(expr, &mut env);
        match value {
            Ok(v) => println!("{v}"),
            Err(e) => {
                dbg!(e);
            }
        }
    }
}
