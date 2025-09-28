pub mod ast;
pub mod error_handling;
pub mod heap;
pub mod keywords;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod runtime;
pub mod tokens;
pub mod types;
pub mod value;

use clap::{Parser, Subcommand};
use std::fs;
use std::path::PathBuf;

use crate::types::type_check_program;

#[derive(Parser)]
#[command(name = "omen")]
#[command(about = "The Omen programming language interpreter")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run an Omen script file
    Run {
        /// Path to the .omen file to execute
        file: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { file } => {
            if let Err(e) = run_file(file) {
                eprintln!("Error: {e}");
                std::process::exit(1);
            }
        }
    }
}

fn run_file(file_path: PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    // Check file extension
    if file_path.extension().unwrap_or_default() != "omen" {
        return Err("File must have .omen extension".into());
    }

    // Read the file
    let source = fs::read_to_string(&file_path)?;

    // Lex, parse, and interpret
    let mut lexer = crate::lexer::Lexer::new(source);

    let tokens = lexer.tokenize();
    let mut parser = crate::parser::Parser::new(tokens);
    let program = parser.parse();

    if let Err(type_error) = type_check_program(&program) {
        eprintln!("Type error: {type_error}");
        std::process::exit(1);
    }

    let mut interpreter = crate::runtime::Interpreter::new();
    interpreter.interpret(&program)?;

    Ok(())
}
