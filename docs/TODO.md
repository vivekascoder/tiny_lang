## Things to do to finish the project

### To achieve: Inbuilt functions / print() / function calls.

- [x] implement tokenizing and parsing of function definations.
- [x] Finish the `environment` to store the scoped data for a function during the call.
- [x] Implement the Function type in the ExprResult.

### To achieve: Structs

- [ ] implement tokenization / parsing of struct definations.
- [ ] parse struct's initilization.
- [ ] store them in the environment.

### To achieve: Modules / Multiple file / project structure

- [ ] Have some info on the module tied along with the Lexer -> Parser -> Enviromment
- [x] Write a CLI to manage the whole interpreter.
- [ ] Some way to parse imports inside a folder and interpret them.

### Migrating lexer and parser.

- [x] Write lexer in logos.
- [ ] Write parser in chumsky.

### Compiler.

> The compiler will compile to some IR and then we'll have an interpreter to interpret that IR. We might also use cranelift or LLVM to compile the IR down to machine code.

- [ ] Design the IR.
- [ ] Transpile to IR
