## Expressions

What are expressions?

1. Literals (Bool, Numbers, etc),
2. Identifiers (Varibles),
3. Function calls,
4. Infix, Prefix (++3, !4, 4- 3)

What should the `eval_expr(e: Expr)` return?

1. Literals -> Literal.
2. Idenfiers
   1. Variables -> Literal
3. Function calls -> The return type of the function.
   1. Return type is void -> Void,
   2. Return type is literal -> Literal,
   3. Return type is, ... for now nothing else is supported.
4. Infix, Prefix -> Literal

What should `eval_infix_expr(e: InfixExpr)` do?

1. Left expr is literal
2. Anything else -> parse expression again

What should my environment store?

1. Variables
   - Name, Value
2. Functions
   - Name, parameter, body
3. Structs
   - field name, field value

How does function looks like in tiny_lang?

```ts
fun calculate_something(a: usize, b: usize) => bool {
    if (a > b) {
        return true;
    } else {
        return false;
    }
}
```

What can be inside a function / statement types.

1. let, declare a variable.
2. assign or mutate a variable.
3. If, else.
4. return statement.

Will we require nested scopes in interpreter?

```
{
    scope: {
        "something": 23,
        "another": 34,
    },
    parent: {
        scope: {
            "something": 23,
            "another": 34,
        },
        parent: null,
    }
}
```

How to store functions?

- Can expressions result in a function?
  - false
- What type is getting stored in the memory?

How tihe recursive `ExprResult::Return<Box<ExprResult>>` should be handled?

For syntax highlighting
https://github.com/damirka/move-syntax/blob/main/syntaxes/move.tmLanguage.json

Parse isize or usize based on type.

### Compiler

1. If let is `usize` and val is `isize`

Not going to implement a compiler that compile to some IR and a VM that evaluates this IR until I can some interesting reference impls.

## Things that are remaining.

[TODOs](./TODO.md)
https://github.com/zesterer/ariadne/tree/main
hhttps://github.com/APLanguage/aplang-rs/blob/new-debut/src/parsing/ast/declarations.rsttps://github.com/maciejhirsz/logos

## Parts

- Lexer
- Parser
- Interpreter
- IR
- VM
- Codegen using LLVM/cranelift/binaryen

## Possible routes to take moving forward.

1. Writing a interpreter based VM.
2. LLVM
3. Cranelift
4. ❌ Wasm
   It'll require a lot of work as WASM has pretty simple data types, being able to map complex datatypes into WASM vec.

## Tiny IR

### IR for let statement.

#### simple function

```
fun something(a: isize, b: isize) => isize {
    let d = 10
    let c = a + b + d;
    return c;
}
```

```ll
define i32 @something(i32 %a, i32 %b) {
    %c = add i32 %a, %b
    ret i32 %c
}
```

#### simple if-else statements.

```
fun something(a: isize, b: isize) => isize {
    if (a > b) {
        return a;
    } else {
        return b;
    }
}
```

```ll
define i32 @something(i32 %a, i32 %b) {
    if_1:
    %c = icmp sgt i32 %a, %b
    br i1 %c, label %if_1, label %else_1

    else_1:
    ret i32 %b
}
```

### Where should we create blocks?

- If-else conditions.

## WASM

### Functions

```

(func (param i32) (param i32) (result i64) <function body>)

```

## Simple function to emit using codegen

```rs
fn sum(a: u64, b: u64) -> u64 {
    return a + b;
}

fn main() {
    let s = sum(1, 2);
    println!("{}", s);
}
```

## TinyLang new syntax to match llvm ir more closely.

```
// constants
const a: i32 = 10;


// functions
fun larger(a: i32, b: i32) => i32 {
    if (a > b) {
        return a;
    } else {
        return b;
    }
}

// main entry point of the program
fun main() => i32 {
    let a: i32 = 10;
    let b: i32 = 20;
    let c: i32 = larger(a, b);
    println(c);

    return 0;
}
```

## Resources

- https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/docs/ir.md
- https://llvm.org/docs/LangRef.html
- https://cs.lmu.edu/~ray/notes/ir/
- https://radu-matei.com/blog/practical-guide-to-wasm-memory/
- https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/
- https://github.com/jakubDoka/stackery
- https://hackmd.io/@Kixiron/rJS7_OEbw
- https://github.com/zesterer/tao/tree/master
- https://github.com/DenialAdams/roland
- https://crates.io/crates/wasm-encoder
- https://github.com/Kixiron/rust-langdev
- https://github.com/msakuta/inkwell-ruscal/blob/master/src/parser.rs
- https://github.com/Rodrigodd/chasm-rs/blob/master/chasm-rs/src/compiler.rs#L663
- https://github.com/mental32/monty/blob/master/montyc_codegen/src/structbuf.rs
