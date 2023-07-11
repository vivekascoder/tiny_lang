# Tiny Lang

Tiny Lang is a type safe general purpose programming language

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
