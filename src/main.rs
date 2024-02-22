use lexer::Lexer;
use std::env;
use std::fs;
use std::process;

pub mod lexer;

fn main() {
    let args: Vec<String> = env::args().collect();
    // dbg!(&args);

    if args.len() > 2 {
        println!("Usage: rlox [script]");
        process::exit(64);
    } else if args.len() == 2 {
        run_file(&args[1]);
    } else {
        run_repl();
    }
}

fn run_file(file_path: &str) {
    let source = fs::read_to_string(file_path).expect("Something went wrong reading the file");
    let has_error = run(&source);
    if has_error {
        process::exit(65);
    }
}

fn run_repl() {
    loop {
        println!("> ");
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        run(&input);
    }
}

fn run(source: &str) -> bool {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.scan_tokens();
    for token in tokens {
        println!("{:?}", token);
    }
    lexer.had_error
}
