use std::{io::stdin, rc::Rc};

use crate::eval::LayResult;

use super::{eval, Environment, Expr};
use anyhow::{bail, Result};

pub(crate) type Builtin = fn(&[Expr], &mut Environment) -> Result<Expr>;

#[macro_export]
macro_rules! builtins {
    ( $($name:ident $(as $alias:literal)? ,)* ) => {
        {
            let mut env = std::collections::BTreeMap::new();
            $(
                #[allow(unused)]
                let mut func_name = stringify!($name).to_string();
                $(
                    func_name = $alias.to_string();
                )?
                env.insert(func_name, std::rc::Rc::new(Expr::Builtin($name)));
            )*
            env
        }
    };
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            parent: None,
            bindings: builtins![
                add as "+",
                sub as "-",
                eq  as "=",
                println,
                readln,
                parse_num,
                str,
            ],
        }
    }
}

pub fn add(values: &[Expr], env: &mut Environment) -> Result<Expr> {
    if values.is_empty() {
        bail!("wrong number of args `0` passed to `+`")
    } else if values.len() == 1 {
        match eval(values.first().unwrap(), env)? {
            n @ Expr::Number(_) => Ok(n),
            _ => bail!("expected number but found {values:?}"),
        }
    } else if values.len() == 2 {
        // 80% of the time, this is a binary operation
        let m = eval(&values[0], env)?;
        let n = eval(&values[1], env)?;
        let (Expr::Number(m), Expr::Number(n)) = (m, n) else {
            unreachable!()
        };

        Ok(Expr::Number(m.overflowing_add(n).0))
    } else {
        values
            .iter()
            .map(|v| {
                eval(v, env).and_then(|v| match v {
                    Expr::Number(n) => Ok(n),
                    _ => bail!("expected number but found {v:?}"),
                })
            })
            .collect::<Result<Vec<i32>>>()
            .map(|n| {
                Expr::Number(
                    n.into_iter()
                        .reduce(|acc, x| i32::overflowing_add(acc, x).0)
                        .unwrap(),
                )
            })
    }
}

pub fn sub(values: &[Expr], env: &mut Environment) -> Result<Expr> {
    if values.is_empty() {
        bail!("wrong number of args `0` passed to `-`")
    } else if values.len() == 1 {
        match eval(values.first().unwrap(), env) {
            Ok(Expr::Number(n)) => Ok(Expr::Number(-n)),
            Ok(_) => bail!("invalid arg passed to `-` {values:?}"),
            Err(e) => Err(e),
        }
    } else if values.len() == 2 {
        // 80% of the time, this is a binary operation
        let m = eval(&values[0], env)?;
        let n = eval(&values[1], env)?;
        let (Expr::Number(m), Expr::Number(n)) = (m, n) else {
            unreachable!()
        };

        Ok(Expr::Number(m.overflowing_sub(n).0))
    } else {
        values
            .iter()
            .map(|v| {
                eval(v, env).and_then(|v| match v {
                    Expr::Number(n) => Ok(n),
                    _ => bail!("expected number but found {v:?}"),
                })
            })
            .collect::<Result<Vec<i32>>>()
            .map(|n| {
                Expr::Number(
                    n.into_iter()
                        .reduce(|acc, x| i32::overflowing_sub(acc, x).0)
                        .unwrap(),
                )
            })
    }
}

pub fn eq(values: &[Expr], env: &mut Environment) -> Result<Expr> {
    if values.is_empty() {
        Ok(Expr::Bool(true))
    } else if values.len() == 2 {
        Ok(Expr::Bool(eval(&values[0], env)? == eval(&values[1], env)?))
    } else {
        let values = values
            .iter()
            .map(|v| eval(v, env))
            .collect::<Result<Vec<Expr>>>()?;

        let first = values.first().unwrap().clone();

        Ok(Expr::Bool(values.into_iter().all(|v| v == first)))
    }
}

pub fn println(values: &[Expr], env: &mut Environment) -> Result<Expr> {
    if values.len() != 1 {
        bail!("wrong number of args `{}` passed to println ", values.len())
    }

    eval(&values[0], env).and_then(|v| {
        match v {
            Expr::String(s) => println!("{s}"),
            _ => bail!("only strings can be printed"),
        }
        Ok(Expr::Nil)
    })
}

pub fn readln(values: &[Expr], _env: &mut Environment) -> Result<Expr> {
    if values.len() != 0 {
        bail!("wrong number of args `{}` passed to println ", values.len())
    }

    let stdin = stdin();

    let mut input = String::new();
    let _input_bytes = stdin.read_line(&mut input)?;

    input = input.trim_end().to_string();

    Ok(Expr::String(input))
}

pub fn parse_num(values: &[Expr], env: &mut Environment) -> Result<Expr> {
    if values.len() != 1 {
        bail!("wrong number of args `{}` passed to parse_num", values.len())
    }

    let Expr::String(s) = eval(&values[0], env)? else {
        bail!("expeceted string but found {values:?}")
    };

    match s.parse::<i32>() {
        Ok(n) => Ok(Expr::Result(Rc::new(LayResult::Success(Expr::Number(n))))),
        Err(e) => Ok(Expr::Result(Rc::new(LayResult::Fail(Expr::String(
            e.to_string(),
        ))))),
    }
}

pub fn str(values: &[Expr], env: &mut Environment) -> Result<Expr> {
    Ok(Expr::String(
        values
            .into_iter()
            .map(|v| eval(v, env))
            .collect::<Result<Vec<Expr>>>()?
            .into_iter()
            .map(|v| format!("{v}"))
            .collect::<Vec<String>>()
            .join(""),
    ))
}
