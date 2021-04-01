# eilang 0.4.0

A rewrite of eilang in Rust to improve the performance of the interpreter, while also reimagining the memory model for
both improved access times and a smaller memory footprint.

It is very much a work in progress.

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