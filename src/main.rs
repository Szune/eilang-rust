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
pub mod typechecker;

use lexer::Lexer; // use Lexer struct, otherwise write lexer::Lexer
use parser::Parser;
use compiler::Compiler;
use interpreter::Interpreter;
use typechecker::TypeChecker;
use std::borrow::Borrow;

fn main() {
    // implement different types of chars for different encodings? or just one 'char'
    // that deals with everything as regular bytes?
    let lexer_for_parser = Lexer::new(String::from(
        r#"
        /*
        fn add(x: int, y: int) -> int { return x + y; }
        fn a_2(x: int) -> int { return x + 1359; }
        fn poutine(x: string) -> string { return x + "yeppo"; }
        fn im_void() -> any { return "Hello"; }
        fn add_int_to_str(i: int, s: string) -> string { return s + i; }
        println(add_int_to_str(139, "this value is not pi "));
        println(add(10, a_2(5)));
        println(poutine("it is " + 5 + " "));
        //println(add("hej" + 5,9));
        //println(add("0" + s,9));
        println(a_2(195));
        */
        /*
        println(a_2("hel"));
        println(add("hel"));
        println(add("hel", 0));
        */
        /*
        println(add(10 - 7, 5 - 7));
        println("subraction: " + (10 - 7));
        println("this is some text being printed");
        println(add(1,1));
        println();
        println(5);
        */
        println(5);
        test := 3;
        if test != 3 {
            println("variable was not 3");
        } else {
            println("variable was 3");
        }

        if 5 == 4 {
            println("should not reach this part");
        } else {
            println("should reach this part");
        }
        if 5 == 5 {
            println("5 == 5 is true");
        }

        if 4 > 5 {
            println("there's a bug (4 > 5)");
        }

        if 5 < 4 {
            println("there's a bug (5 < 4)");
        }

        if 5 > 4 {
            println("5 > 4");
        }

        if 4 < 5 {
            println("4 < 5");
        }

        if 201 >= 200 && 201 >= 201 {
            println("201 >= 200 && 201 >= 201");
        }

        if 201 <= 201 && 200 <= 201 {
            println("201 <= 201 && 200 <= 201");
        }

        if 10 > 5 || 5 > 10 {
            println("10 > 5 || 5 > 10");
        }
        "#)); // "fn add() -> int { return 5 + 10; }"
    let ast = Parser::parse(lexer_for_parser);
    //println!("Parsed AST: {:#?}", ast);
    let result = TypeChecker::check(&ast);
    if !result.success.get() {
        println!("Type checker result: {}\n{}", result.success.get(), result.errors.borrow_mut().join("\n"));
        return;
    }
    //println!("Compiling");
    let env = Compiler::compile(ast);
    //println!("{:#?}", env.get_function(".main".into()).code.iter().enumerate().collect::<Vec<(usize,&ops::OpCodes)>>());
    //println!("Running");
    Interpreter::interpret(env);
}
