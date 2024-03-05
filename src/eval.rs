use anyhow::{bail, Result};
use std::{collections::BTreeMap, fmt::Display, rc::Rc};

pub mod builtins;
use self::builtins::Builtin;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Nil,
    Bool(bool),
    Number(i32),
    Symbol(String),
    String(String),
    Lambda { args: Vec<String>, body: Rc<Expr> },
    Builtin(Builtin),
    Result(Rc<LayResult>),
    List(Vec<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum LayResult {
    Success(Expr),
    Fail(Expr),
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
            Expr::Builtin(b) => write!(f, "{b:#?}"),
            Expr::Result(v) => write!(f, "{v:#?}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    bindings: BTreeMap<String, Rc<Expr>>,
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

    pub fn lookup(&self, symbol: &str) -> Result<Expr> {
        match self.get(symbol) {
            Some(expr) => Ok(expr.as_ref().clone()),
            None => bail!("undefined value {symbol:?}"),
        }
    }

    pub fn set(&mut self, symbol: String, value: Expr) -> Option<Rc<Expr>> {
        self.bindings.insert(symbol, Rc::new(value))
    }

    pub fn new_child(&self) -> Self {
        Self {
            parent: Some(Rc::new(self.clone())),
            bindings: Default::default(),
        }
    }
}

pub(crate) fn eval(expr: &Expr, env: &mut Environment) -> Result<Expr> {
    match expr {
        Expr::Nil
        | Expr::Builtin(_)
        | Expr::Bool(_)
        | Expr::Number(_)
        | Expr::String(_)
        | Expr::Result(_)
        | Expr::Lambda { .. } => Ok(expr.to_owned()),
        Expr::Symbol(ref s) => env.lookup(s),
        Expr::List(list) => match &list[..] {
            [] => Ok(Expr::Nil),

            // if
            [Expr::Symbol(op_symbol), condition, consequent, alternate] if op_symbol == "if" => {
                match eval(condition, env)? {
                    Expr::Bool(true) => eval(consequent, env),
                    Expr::Bool(false) => eval(alternate, env),
                    x => bail!("expected a bool but found {x:?}"),
                }
            }

            // unwrap
            [Expr::Symbol(op_symbol), expr, fallback] if op_symbol == "unwrap" => {
                match eval(expr, env)? {
                    Expr::Result(r) => match r.as_ref() {
                        LayResult::Success(e) => eval(&e, env),
                        LayResult::Fail(_) => eval(fallback, env),
                    },
                    x => bail!("expected a result but found {x:?}"),
                }
            }

            // map
            [Expr::Symbol(op_symbol), func, expr] if op_symbol == "map" => match eval(expr, env)? {
                Expr::Result(r) => match r.as_ref() {
                    LayResult::Success(e) => {
                        let f = eval(func, env)?;
                        Ok(Expr::Result(Rc::new(LayResult::Success(apply(
                            &f,
                            &[e.clone()],
                            env,
                        )?))))
                    }
                    LayResult::Fail(_) => eval(expr, env),
                },
                x => bail!("expected a result but found {x:?}"),
            },

            // try
            [Expr::Symbol(op_symbol), expr, success, fail] if op_symbol == "try" => {
                match eval(expr, env)? {
                    Expr::Result(r) => match r.as_ref() {
                        LayResult::Success(e) => {
                            let f = eval(success, env)?;
                            Ok(apply(&f, &[e.clone()], env)?)
                        }
                        LayResult::Fail(e) => {
                            let f = eval(fail, env)?;
                            Ok(apply(&f, &[e.clone()], env)?)
                        }
                    },
                    x => bail!("expected a result but found {x:?}"),
                }
            }

            // def
            [Expr::Symbol(op), Expr::Symbol(s), value] if op == "def" => {
                let value = eval(value, env)?;
                env.set(s.to_string(), value);
                Ok(Expr::Nil)
            }

            // let
            [Expr::Symbol(op), Expr::List(s), body] if op == "let" => {
                let bindings = s
                    .iter()
                    .map(|kv_list| match kv_list {
                        Expr::List(pair) => match &pair[..] {
                            [Expr::Symbol(s), value] => Ok((s, eval(value, env)?)),
                            e => bail!("found invalid binding {e:?}"),
                        },
                        e => bail!("found invalid binding {e:?}"),
                    })
                    .collect::<Result<Vec<(&String, Expr)>>>()?;

                let mut new_env = env.new_child();
                for (k, v) in bindings {
                    new_env.set(k.to_owned(), v.clone());
                }

                eval(body, &mut new_env)
            }

            // fn
            [Expr::Symbol(op), Expr::List(args), body] if op == "fn" => args
                .iter()
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

            // quote
            [Expr::Symbol(op), arg] if op == "quote" => Ok(arg.clone()),

            // eval
            [Expr::Symbol(op), arg] if op == "eval" => {
                eval(arg, env).and_then(|out| eval(&out, env))
            }

            // sexp
            [op, rest @ ..] => {
                let op = eval(op, env)?;
                apply(&op, rest, env)
            }
        },
    }
}

fn apply(operator: &Expr, rest: &[Expr], env: &mut Environment) -> Result<Expr> {
    match operator {
        Expr::Lambda { args, body } => {
            let mut new_env = env.new_child();
            for (i, arg) in args.iter().enumerate() {
                new_env.set(arg.to_owned(), eval(&rest[i], env)?);
            }
            eval(body, &mut new_env)
        }
        Expr::Builtin(f) => f(rest, env),
        _ => unreachable!("i don't know {operator:#?} {rest:#?} {env:#?}"),
    }
}
