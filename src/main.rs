use colored::Colorize;
use serde_json;
use std::env;
use std::fs;
use std::process;

mod ast;
mod interpreter;
mod lexer;
mod object;
mod parser;
mod reporter;
mod resolver;
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
    run(&source, false);
}

fn run_repl() {
    loop {
        println!("{} ", "🦀>".purple().bold());
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        run(&input, true);
    }
}

fn run(source: &str, is_repl: bool) {
    let lexer_reporter = reporter::Reporter::new();
    let mut lexer = lexer::Lexer::new(source, lexer_reporter);
    let tokens = lexer.scan_tokens();

    if lexer.reporter.had_error {
        return exit(65, is_repl);
    }

    let parser_reporter = reporter::Reporter::new();
    let mut parser = parser::Parser::new(tokens, parser_reporter);
    let statements = parser.parse();
    match statements {
        Ok(mut stmts) => {
            let ast_json = serde_json::to_string_pretty(&stmts).unwrap();
            println!("{}", ast_json);

            let resolver_reporter = reporter::Reporter::new();
            let mut resolver = resolver::Resolver::new(resolver_reporter);
            let resolve_had_error = resolver.resolve_stmts(&mut stmts);

            if resolve_had_error {
                return exit(65, is_repl);
            }

            let mut interpreter = interpreter::Interpreter::new(resolver);
            let value = interpreter.interpret(&mut stmts);
            match value {
                Ok(value) => {
                    println!("{} {:?}", "==>".blue(), value);
                }
                Err(_err) => exit(70, is_repl),
            }
        }
        Err(_err) => exit(65, is_repl),
    }
}

fn exit(code: i32, is_repl: bool) {
    if !is_repl {
        process::exit(code);
    }
}
