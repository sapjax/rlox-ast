use lexer::Lexer;
use std::env;
use std::fs;
use std::process;

mod ast;
mod lexer;
mod parser;
mod reporter;
mod token;

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
    let has_error = run(&source, file_path);
    if has_error {
        process::exit(65);
    }
}

fn run_repl() {
    loop {
        println!("> ");
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        run(&input, "repl");
    }
}

fn run(source: &str, file_path: &str) -> bool {
    let lexer_reporter = reporter::Reporter::new();
    let mut lexer = Lexer::new(source, lexer_reporter);
    let tokens = lexer.scan_tokens();

    if lexer.reporter.had_error {
        return true;
    }

    let parser_reporter = reporter::Reporter::new();
    let mut parser = parser::Parser::new(tokens, parser_reporter);
    let expr: Result<ast::Expr, parser::ParseError> = parser.parse();
    match expr {
        Ok(expr) => println!("{}", expr),
        Err(err) => {
            miette::Error::new(err)
                .with_source_code(miette::NamedSource::new(file_path, source.to_string()));
        }
    }
    parser.reporter.had_error
}
