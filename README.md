# Tiny Lang

A simple frontend for LLVM

## Specification of the language.

```
printf("Hello World");
```

```
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
