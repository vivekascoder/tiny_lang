# Tiny Lang

Tiny Lang is a toy programming language. It'll soon support compiling to WASM.

## Install

```bash
git clone https://github.com/vivekascoder/tiny_lang
cd tiny_lang
cargo install --path .
```

## Run program

```bash
tiny_lang interpret ./examples/main.tiny
```

## Examples

### Conditions

```
// Function to return greatest among three passed numbers.
fun greatest(a: usize, b: usize, c: usize) => usize {
    if (a > b) {
        if (a > c) {
            return a;
        } else {
            return c;
        }
    } else {
        if (b > c) {
            return b;
        } else {
            return c;
        }
    }
}

// Call and print the result.
print(greatest(34, 46, 2));
```

### Functions

```
let a = 454 + 3636 * 3;
let b = 45 / 3;

fun something(a: usize) => usize {
    let something = 353;
    return something;
}

let re = something(35);
print(re);
```

### For more exmaples.

Check out the [examples](./examples/) folder.
