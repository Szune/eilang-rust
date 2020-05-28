pub mod token; // define token as a public module
pub mod lexer; // define lexer as a public module
pub mod parser;
pub mod ast;
pub mod compiler;
pub mod values;
pub mod ops;
pub mod function;
pub mod env;
pub mod interpreter;

use lexer::Lexer; // use Lexer struct, otherwise write lexer::Lexer
use parser::Parser;
use compiler::Compiler;
use interpreter::Interpreter;

fn main() {
    let lexer_for_parser = Lexer::new(String::from(
        "fn add(x: int, y: int) -> int { return x + y; }\
        fn a_2(x: int) -> int { return x + 1359; } \
        println(add(5,9));\
        println(a_2(195));\
        println();\
        println(5);")); // "fn add() -> int { return 5 + 10; }"
    let ast = Parser::parse(lexer_for_parser);
    //println!("Parsed AST: {:?}", ast);
    let env = Compiler::compile(ast);
    //println!("{:?}", env.get_function(".main".into()).code);
    Interpreter::interpret(env);
}