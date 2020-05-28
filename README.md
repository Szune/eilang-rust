# Warning
Everything in this repo is a work in progress, I don't know Rust well enough yet to write good Rust, this is currently a learning experience. At some point this will hopefully be the main repo for 'eilang'.

# eilang-rust
A rewrite of eilang in Rust to improve the performance of the interpreter, while also reimagining the memory model for both improved access times and a smaller memory footprint.

## Working right now
```eilang
fn add(x: int, y: int) -> int { // function declarations
	return x + y;
}

println(add(5,9)); // nested function calls, comments, prints "Integer(14)" (a reasonable println function is on the way)
```