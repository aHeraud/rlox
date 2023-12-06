use std::process::exit;
use rlox::Lox;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 2 {
        println!("Usage: lox [script]");
        exit(64)
    } else {
        let mut lox = Lox::new();
        if args.len() == 1 {
            lox.run_prompt();
        } else {
            let result = lox.run_file(&args[1]);
            if let Err(err) = result {
                eprintln!("{}", err);
                exit(70);
            }
        }
    }
}
