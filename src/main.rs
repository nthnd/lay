mod eval;
mod read;

use std::path::PathBuf;

use eval::Environment;
use pest::Parser;
use rustyline::{
    config::Configurer, error::ReadlineError, validate::Validator, Completer, Helper, Highlighter,
    Hinter,
};

use crate::read::Grammar;

fn main() {
    if let Some(path) = std::env::args().nth(1) {
        file(path.into())
    } else {
        repl()
    }
}

pub fn file(path: PathBuf) {
    let contents = std::fs::read_to_string(path).unwrap();

    let mut env = Environment::default();

    let exprs = read::read(&contents).unwrap();

    for expr in exprs {
        let value = eval::eval(&expr, &mut env);
        match value {
            Ok(_) => {
                // println!("{v}")
            }
            Err(e) => {
                println!("{e}");
                break;
            }
        }
    }
}
pub fn repl() {
    #[derive(Completer, Highlighter, Hinter, Helper)]
    struct LayValidator;
    impl Validator for LayValidator {
        fn validate(
            &self,
            ctx: &mut rustyline::validate::ValidationContext,
        ) -> rustyline::Result<rustyline::validate::ValidationResult> {
            let input = ctx.input();

            match Grammar::parse(read::Rule::input, input) {
                Ok(_) => Ok(rustyline::validate::ValidationResult::Valid(None)),
                Err(_) => Ok(rustyline::validate::ValidationResult::Invalid(None)),
            }
        }

        fn validate_while_typing(&self) -> bool {
            true
        }
    }

    let h = LayValidator;
    let mut rl = rustyline::Editor::new().unwrap();
    rl.set_helper(Some(h));
    rl.set_edit_mode(rustyline::EditMode::Vi);

    let mut env = Environment::default();
    loop {
        let line = match rl.readline("> ") {
            Ok(line) => line,
            Err(ReadlineError::Eof | ReadlineError::Interrupted) => {
                break;
            }
            Err(e) => {
                panic!("{e}")
            }
        };

        let exprs = match read::read(&line) {
            Ok(expr) => expr,
            Err(e) => {
                eprintln!("{e}");
                continue;
            }
        };

        for expr in exprs {
            let value = eval::eval(&expr, &mut env);
            match value {
                Ok(v) => println!("{v}"),
                Err(e) => {
                    println!("{e}");
                }
            }
        }
    }

    println!("bye <3")
}
