# Kava - A Simple Programming Language

## Introduction

Kava is a programming language inspired by Kotlin with elements borrowed from Java and JavaScript. It is statically typed, interpreted, and supports functions as first-class citizens. It is designed to be simple and easy to learn, with a syntax that is similar to other popular languages.

## Syntax

### Types

Kava supports the following types: int, string, bool, lists of a specific type (including lists of functions), and functions.

Examples of type annotations:

- `int`
- `string`
- `int[]` - list of integers
- `F(int, string) -> bool[]` - function with two parameters, int and string. Returns a list of booleans.

### Literals

Standard literals:

- `"string";`
- `10;`
- `List.of(1, 2, 3);`

For functions:

- `(a: int, b: bool[]): int -> { return a; };` - anonymous function of type `F(int, bool[]) -> int`

### Variables

Variables are declared as follows:

- `let x: <TYPE> = ...`
- `const y: <TYPE> = ...` - cannot be reassigned
- `let z: <TYPE>;` - uninitialized variable

An uninitialized variable has a default value based on its type:

- string -> ""
- int -> 0
- bool -> false
- list -> []
- function -> Exception (must be initialized)

A simple type inference is implemented. If the type of a variable can be inferred from the context, it doesn't need to be explicitly stated:

```kava
// x is of type int
auto x = 10;

// y is of type F(int) -> int
auto y = (a: int): int -> { return x; };
```

### Comparison operators

Comparisons are structural:

```kava
List.of(1, 2) == List.of(1, 2); // true
List.of(List.of(1, 2), List.of(3, 4)) == List.of(List.of(1, 2), List.of(3, 4)) // true

let x = "x";
let y = "x";

x == y // true
true == false // false
```

Functions cannot be compared. Operators "<", ">", "<=", ">=" are only supported for ints and strings. For strings, comparisons are lexicographical.

### Arithmetic operators

Standard arithmetic operations are supported.

For ints: addition, subtraction, multiplication, division, modulo. <br>
For lists: addition (concatenation). <br>
For strings: addition (concatenation).

Supported syntactic sugar:

- `+=`
- `-=`
- `*=`
- `/=`
- `%=`

### Logical operators

Not lazy.

- `&&`
- `||`
- `!`

### Built-in Functions

- `print(x)` - prints results of expresssion `x`, where `x` can be of any type
- `len(list)` - returns the length of the list

### Flow Control

Supports `while`, `if` (with `elif` and `else`):

```kava
while (<BExp>) {

}

if (<BExp>) {

} elif (<BExp>) {

} else {

}
```

Loop for iterating over list elements:

```kava
for (const <IDEN> of <LIST>) {
    // iterates the variable (which is const) over elements of the list
}
```

### Lists

Indexing (including negative values) and ranges.

```kava
len(List.of()) == 0 // true
len(List.of("a")) == 1 // true

List.of("a", "b")[0] == "a" // true
List.of("a", "b")[-1] == "b" // true

x..y == List.of(x, x+1, ..., y-1, y) // when x < y
x..y == List.of(x, x-1, ..., y+1, y) // when x > y
// x and y must be ints, they can be negative
```

### Functions

Supports static binding, recursion, nested functions, pass-by-value, variable shadowing, and global variables.

Functions must be declared before use.

```kava
const global: int = 0;

fun foo(x: int, const y: string): string {
    fun bar(y: string): void {
        y += "b";
        print(y);
    }

    bar(y);
    print(y); // y remains unchanged
    return y;
}
```

### Anonymous Functions and Closures

```kava
fun foo(): F(int) -> int {
    let y: int = 0;
    const f: F(int) -> int = (x: int): int -> { y++; return x+y; }
    return f;
}
```

## Technical Details

Implemented in Haskell using the BNF Converter tool as part of the course "Programming languages and paradigms" at the University of Warsaw.

It consists of two parts: an interpreter and a type checker. Both use monad transformers ReaderT, StateT, and ExceptT. There is one shift/reduce conflict in the grammar, which occurs in function types that return lists, for example:

```
F(int) -> int[]
```

The issue is that it's unclear whether to interpret this as `(F(int) -> int)[]` or as `F(int) -> (int[])`. By default, it is treated as the latter, which was the initial intention. Therefore, this conflict is harmless.
