mod eval;
mod read;

use eval::Environment;
use rustyline::config::Configurer;

fn main() {
    let mut rl = rustyline::DefaultEditor::new().unwrap();
    rl.set_edit_mode(rustyline::EditMode::Vi);

    let mut env = Environment::default();

    loop {
        let line = rl.readline("> ").unwrap();

        let expr = match read::read(&line) {
            Ok(expr) => expr,
            Err(e) => {
                eprintln!("{e}");
                continue;
            }
        };

        let value = eval::eval(expr, &mut env);
        match value {
            Ok(v) => println!("{v}"),
            Err(e) => {
                println!("{e}");
            }
        }
    }
}
