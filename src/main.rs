mod eval;
mod read;

use std::path::PathBuf;

use eval::Environment;
use rustyline::{config::Configurer, error::ReadlineError};

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
            Ok(v) => println!("{v}"),
            Err(e) => {
                println!("{e}");
            }
        }
    }
}
pub fn repl() {
    let mut rl = rustyline::DefaultEditor::new().unwrap();
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
