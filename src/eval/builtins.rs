use super::{eval, Environment, Expr};
use anyhow::{bail, Result};

pub(crate) type Builtin = fn(&[Expr], &mut Environment) -> Result<Expr>;

#[macro_export]
macro_rules! builtins {
    ( $($name:ident $(as $alias:literal)? ,)* ) => {
        {
            let mut env = std::collections::HashMap::new();
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
    if values.len() == 0 {
        bail!("wrong number of args `0` passed to `+`")
    } else {
        values
            .iter()
            .map(|v| {
                eval(v.clone(), env).and_then(|v| match v {
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
    if values.len() == 0 {
        bail!("wrong number of args `0` passed to `-`")
    } else if values.len() == 1 {
        match eval(values.first().unwrap().clone(), env) {
            Ok(Expr::Number(n)) => Ok(Expr::Number(-n)),
            Ok(_) => bail!("invalid arg passed to `-` {values:?}"),
            Err(e) => Err(e),
        }
    } else {
        values
            .iter()
            .map(|v| {
                eval(v.clone(), env).and_then(|v| match v {
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
    } else {
        let values = values
            .iter()
            .map(|v| eval(v.clone(), env))
            .collect::<Result<Vec<Expr>>>()?;

        let first = values.first().unwrap().clone();

        Ok(Expr::Bool(values.into_iter().all(|v| v == first)))
    }
}

