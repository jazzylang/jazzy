# jazzy
[![Rust](https://github.com/jazzylang/jazzy/actions/workflows/rust-ci.yml/badge.svg)](https://github.com/jazzylang/jazzy/actions/workflows/rust-ci.yml)

![Jazzy REPL header ASCII art](https://i.imgur.com/aG5ROKh.png)

A better programming language (than my last one)

## Installation
To install jazzy, clone the repository:
```bash
$ git clone https://github.com/jazzylang/jazzy.git
```
In the newly created `jazzy` directory, build the compiler:
```bash
$ cargo build --release
```
The compiler will now be located at `jazzy/target/release/jazzy`. You can either move the compiler to a folder in your system's `$PATH`, or add `jazzy/target/release` to your system's `$PATH` variable.

Once the jazzy compiler is in your `$PATH`, you can invoke the compiler without any arguments or options to drop into the interactive REPL:
```bash
$ jazzy
```
or invoke it with the path to a jazzy source file to compile and execute it:
```bash
$ jazzy hello-world.jzy
```

To learn more about options and arguments that can be passed into the compiler, invoke the compiler with the `-h` option:
```bash
$ jazzy -h
```

## Features
jazzy currently only supports collections of variable assignments and expressions on integers, floating-point numbers, and booleans. To execute a jazzy program, either type it out in the REPL one line at a time:

```
=^..^= let x := 1;
=^..^= x + 1 
2

=^..^=
```
or create a `.jzy` file with a single expression in it:
```
// in file expression.jzy
1 < 3 and 99 >= 100
```
and invoke the compiler on it:
```
$ jazzy expression.jzy
false
```

### Variables
Variables can be declared as such:
```
let x := 1;
```

The name of your variable is called an "identifier". The idiomatic style for jazzy variable identifiers is not snake case or camel case, like in a lot of common programming languages, but what many people call "kebab case":
```
let some-variable := 3.14;
```

An interesting consequence of this decision is that all of our binary operators (think +, -, etc) need to be surrounded by whitespace. Otherwise, we wouldn't know the difference between subtracting a variable `b` from a variable `a` and referencing a variable `a-b`!

Variables are immutable by default, meaning once you declare them, you cannot assign them a new value.

![Multiple assignments to immutable variable error](https://i.imgur.com/VkelC0c.png)

To declare a variable as mutable, allowing you to assign it a new value later, simply add a `mut` after the `let`:
```
let mut x := 1;
x := 2;
```

There are lots of situations where the jazzy compiler can figure out what the type of your variable is by itself. However, if you run into a situation where it can't (or you just want to explicitly set it for readability), you can add a type hint:
```
let x (i64) := 1;
```

### Types

Speaking of types, jazzy currently has three categories of them: booleans, integers, and floating-point numbers.

#### Booleans
| Type | Description |
|:----:|:-----------:|
| bool |   Boolean   |

There are two boolean literals: `true` and `false`.

#### Integers
| Type |       Description        |
|:----:|:------------------------:|
| i8   |  Signed 8-bit integer    |
| i16  |  Signed 16-bit integer   |
| i32  |  Signed 32-bit integer   |
| i64  |  Signed 64-bit integer   |
| i128 |  Signed 128-bit integer  |
| u8   | Unsigned 8-bit integer   |
| u16  | Unsigned 16-bit integer  |
| u32  | Unsigned 32-bit integer  |
| u64  | Unsigned 64-bit integer  |
| u128 | Unsigned 128-bit integer |

Integer literal values can be represented in three bases:
```c
// Base 10 (decimal)
100
// Base 2 (binary)
0b1100100
// Base 16 (hexadecimal)
0x64
```

Underscores can also be placed anywhere within the integer literal value (except at the beginning) for readability:
```
=^..^= 1_000_000
1000000

=^..^= 1_
1
```

#### Floating point numbers
| Type | Description  |
|:----:|:------------:|
| f32  | 32-bit float |
| f64  | 64-bit float |

Floating-point literal values can be represented in two ways:
```c
// Traditional
3.14
// Scientific notation
1e5 // = 100000.0
3.14e-1 // = 0.314
```

Underscores can also be placed anywhere within the floating-point literal value (except around the `e` and optional `-` in scientific notation representations) for readability:
```
=^..^= 1_000_000.0
1000000

=^..^= 3_._14e1_0
31400000000
```

### Comments
Comments can be used to explain your code, and are ignored by the compiler, so they have no effect on how your code runs. There are two types of comments:
```
// If the compiler sees two forward slashes back-to-back, it ignores everything on the rest of the line

[[
|| If the compiler sees two left square brackets back-to-back,
|| it ignores everything until it sees two right square brackets back-to-back
|| (The ||s are optional, I just add them because I think it looks nice)
]]
```
