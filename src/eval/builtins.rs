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
