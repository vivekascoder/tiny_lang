# Tiny Lang

A simple frontend for LLVM

## Specification of the language.

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

```
printf("Hello World");
```

```ts
import "module_name";

const SOME_VAL: i64 = 123;

let var: f64 = 343.4;
let var2: f64 = var + 3.45;

struct Something {
    val: i32
}

fun sum3(a: i32, b: i32, c: i32): i32 {
    return a + b + c;
}

let result: i32 = sum3(1, 2, 3);

fun main(): void {
    printf("Main function");
}

```

## Debug

```rs
 [
    Let(
        Ident(
            "something",
        ),
        Infix(
            Plus,

            Literal(
                SignedInteger(
                    454,
                ),
            ),

            Infix(
                Multiply,
                Infix(
                    Multiply,
                    Literal(
                        SignedInteger(
                            3,
                        ),
                    ),
                    Literal(
                        SignedInteger(
                            4,
                        ),
                    ),
                ),
                Literal(
                    SignedInteger(
                        35,
                    ),
                ),
            ),
        ),
    ),
],
```
