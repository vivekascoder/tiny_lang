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

## Resources

- https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/docs/ir.md
- https://llvm.org/docs/LangRef.html
- https://cs.lmu.edu/~ray/notes/ir/
- https://radu-matei.com/blog/practical-guide-to-wasm-memory/
- https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/
