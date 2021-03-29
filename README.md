# Warning
Everything in this repo is a work in progress, I don't know Rust well enough yet to write good Rust, this is currently a learning experience. At some point this will hopefully be the main repo for 'eilang'.

Writing the type checker is also entirely new to me, so I wouldn't recommend trying to learn anything from it.

# eilang-rust
A rewrite of eilang in Rust to improve the performance of the interpreter, while also reimagining the memory model for both improved access times and a smaller memory footprint.

## Working right now
```eilang
fn add(x: int, y: int) -> int { // function declarations
	return x + y;
}

added := add(3,2); // variable assignment
if add(2,3) == added { // basic if statements
    println("works all right");
} else { // no else if yet
    println("there's a bug");
}

println(add(5,9)); // nested function calls, comments, prints "14"

hello := "hello";
world := "world!";
println($"{hello} {world}"); // string interpolation
```