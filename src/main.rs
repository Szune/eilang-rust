/*
 * eilang is an experimental programming language, this is its compiler and interpreter.
 * Copyright (C) 2021  Carl Erik Patrik Iwarson
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
pub mod ast;
pub mod builtins;
pub mod compiler;
pub mod env;
pub mod function;
pub mod interpreter;
pub mod lexer;
pub mod ops;
pub mod optimizer;
pub mod parser;
pub mod rustfn;
pub mod scope;
pub mod token;
pub mod typechecker;
pub mod types;
pub mod values;

use compiler::Compiler;
use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

fn main() {
    // implement different types of chars for different encodings?
    // or just one 'char' that deals with everything as regular bytes?
    let lexer_for_parser = Lexer::new(String::from(
        r#"
        fn add(x: int, y: int) -> int { return x + y; }

        fn add2(x: int, y: int) {
            println($"x + y = {x + y}");
        }
        
        xyyy := false;
        xyyy = true;
        if xyyy {
            println("helliioooo");
        }

        add2(10,15);
        added := add(3,2);
        if add(2,3) == added {
            println("works all right");
        } else {
            println("there's a bug");
        }

        println("add: " + add(135,2));
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

        if "test!" == "test!" {
            println("\"test!\" == \"test!\"");
        }
        hello := "hello";
        world := "world!";
        println($"{hello} from {world}");
        "#,
    )); // "fn add() -> int { return 5 + 10; }"
    let (mut ast, mut types) = Parser::parse(lexer_for_parser);
    //println!("Parsed AST: {:#?}", ast);
    /*
    let result = TypeChecker::check(&ast);
    if !result.success.get() {
        println!("Type checker result: {}\n{}", result.success.get(), result.errors.borrow_mut().join("\n"));
        return;
    }
    */
    //println!("Compiling");
    //let (ast, success) = typechecker::check_types(ast, &mut types);
    let typecheck = typechecker::check_types(&mut ast, &mut types);
    match typecheck {
        Ok(_) => {
            let mut env = env::Env::new(types);
            builtins::add(&mut env);
            optimizer::optimize(&mut ast);
            let env = Compiler::compile(env, ast);
            //println!("{:#?}", env.get_function(".main".into()).code.iter().enumerate().collect::<Vec<(usize,&ops::OpCodes)>>());
            //println!("Running");
            Interpreter::interpret(env);
        }
        Err(err) => {
            panic!("Type checking failed: {}", err);
        }
    }
}
