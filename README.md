# jazzy
[![Rust](https://github.com/jazzylang/jazzy/actions/workflows/rust-ci.yml/badge.svg)](https://github.com/jazzylang/jazzy/actions/workflows/rust-ci.yml)

A modern statically typed programming language

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
jazzy currently only supports expressions with number and boolean literals. To execute a jazzy expression, either type it out in the REPL and press enter:
```bash
=^..^= 1 + 1
2

=^..^=
```
or create a `.jzy` file with a single expression in it:
```
// in file expression.jzy
1 < 3 and 99 >= 100
```
and invoke the compiler on it:
```bash
$ jazzy expression.jzy
false
```

### Operators
#### Addition (`+`)
| Left Operand | Right Operand |  Output |
|:------------:|:-------------:|:-------:|
|    Integer   |    Integer    | Integer |
|     Float    |     Float     |  Float  |

The addition operator takes two numbers and adds them together:
```
=^..^= 1 + 2
3
```

#### Subtraction (`-`)
| Left Operand | Right Operand |  Output |
|:------------:|:-------------:|:-------:|
|    Integer   |    Integer    | Integer |
|     Float    |     Float     |  Float  |

The subtraction operator takes two numbers and subtracts them:
```
=^..^= 1 - 2
-1
```

#### Multiplication (`*`)
| Left Operand | Right Operand |  Output |
|:------------:|:-------------:|:-------:|
|    Integer   |    Integer    | Integer |
|     Float    |     Float     |  Float  |

The multiplication operator takes two numbers and multiplies them together:
```
=^..^= 1 * 2
2
```

#### Division (`/`)
| Left Operand | Right Operand |  Output |
|:------------:|:-------------:|:-------:|
|    Integer   |    Integer    | Integer |
|     Float    |     Float     |  Float  |

The division operator takes two numbers, divides them, and returns the quotient:
```
=^..^= 1 / 2
0
```
NOTE: The output of the division operator depends on the type of its operands. If the operands are integers, the division operator performs integer division, which truncates the decimal quotient towards zero in order to return an integer. If the operands are floating-point numbers, the division operator does not alter the decimal quotient:
```
=^..^= 1.0 / 2.0
0.5
```

#### Remainder (`%`)
| Left Operand | Right Operand |  Output |
|:------------:|:-------------:|:-------:|
|    Integer   |    Integer    | Integer |
|     Float    |     Float     |  Float  |

The remainder operator takes two numbers, divides them, and returns the remainder:
```
=^..^= 1 % 2
1

=^..^= 4 % 2
0
```

#### Boolean AND (`and`)
| Left Operand | Right Operand |  Output |
|:------------:|:-------------:|:-------:|
|    `bool`    |    `bool`     | `bool`  |

The boolean AND operator takes two booleans and produces a boolean representing whether both operands are `true`:
| Left Operand | Right Operand |  Output |
|:------------:|:-------------:|:-------:|
|    `true`    |     `true`    |  `true` |
|    `true`    |    `false`    | `false` |
|    `false`   |     `true`    | `false` |
|    `false`   |    `false`    | `false` |

```
=^..^= true and true
true
```

#### Boolean OR (`or`)
| Left Operand | Right Operand |  Output |
|:------------:|:-------------:|:-------:|
|    `bool`    |    `bool`     | `bool`  |

The boolean OR operator takes two booleans and produces a boolean representing whether either or both operands are `true`:
| Left Operand | Right Operand |  Output |
|:------------:|:-------------:|:-------:|
|    `true`    |    `true`     | `true`  |
|    `true`    |    `false`    | `true`  |
|    `false`   |    `true`     | `true`  |
|    `false`   |    `false`    | `false` |

```
=^..^= true or false
true
```

#### Less Than (`<`)
| Left Operand | Right Operand |  Output |
|:------------:|:-------------:|:-------:|
|    Integer   |    Integer    | `bool`  |
|     Float    |     Float     | `bool`  |

The less than operator takes two numbers and returns a boolean representing whether the first operand was less than the second:
```
=^..^= 1 < 2
true

=^..^= 1 < 1
false
```

#### Less Than Or Equal To (`<=`)
| Left Operand | Right Operand |  Output |
|:------------:|:-------------:|:-------:|
|    Integer   |    Integer    | `bool`  |
|     Float    |     Float     | `bool`  |

The less than or equal to operator takes two numbers and returns a boolean representing whether the first operand was less than or equal to the second:
```
=^..^= 1 <= 2
true

=^..^= 1 <= 1
true
```

#### Greater Than (`>`)
| Left Operand | Right Operand |  Output |
|:------------:|:-------------:|:-------:|
|    Integer   |    Integer    | `bool`  |
|     Float    |     Float     | `bool`  |

The greater than operator takes two numbers and returns a boolean representing whether the first operand was greater than the second:
```
=^..^= 2 > 1
true

=^..^= 1 > 1
false
```

#### Greater Than Or Equal To (`>=`)
| Left Operand | Right Operand |  Output |
|:------------:|:-------------:|:-------:|
|    Integer   |    Integer    | `bool`  |
|     Float    |     Float     | `bool`  |

The greater than or equal to operator takes two numbers and returns a boolean representing whether the first operand was greater than or equal to the second:
```
=^..^= 2 >= 1
true

=^..^= 1 >= 1
true
```

#### Equal To (`==`)
| Left Operand | Right Operand |  Output |
|:------------:|:-------------:|:-------:|
|       A      |       A       | `bool`  |

The equal to operator takes two values **of the same type** and returns a boolean representing whether they have the same value:
```
=^..^= 1 == 1
true

=^..^= 1 == 2
false
```

#### Not Equal To (`!=`)
| Left Operand | Right Operand |  Output |
|:------------:|:-------------:|:-------:|
|       A      |       A       | `bool`  |

The not equal to operator takes two values **of the same type** and returns a boolean representing whether they do not have the same value:
```
=^..^= 1 != 1
false

=^..^= 1 != 2
true
```

#### Unary Minus (`-`)
| Operand |  Output |
|:-------:|:-------:|
| Integer | Integer |
|  Float  |  Float  |

The unary minus operator takes a single number and returns the negated value of that number:
```
=^..^= -1
-1

=^..^= --1
1
```

#### Unary Boolean NOT (`not`)
| Operand |  Output |
|:-------:|:-------:|
| `bool`  | `bool`  |

The unary boolean NOT operator takes a single boolean and returns the negated value of that boolean:
```
=^..^= not true
false

=^..^= not not true
true
```

### Literals
There are four kinds of literal values you can use in jazzy expressions:

#### Integers
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

#### Floating-Point Numbers
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

#### Boolean
There are two boolean literal values, `true` and `false`.

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
