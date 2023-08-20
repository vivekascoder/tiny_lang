use std::fmt::Display;

use anyhow::{bail, Result};

use crate::ast::{Expr, ExprResult, Literal, Program, Statement};

/**
## Go Code here for the reference

type Definition struct {
    Name string ,OperandWidths []int
}

var definitions = map[Opcode]*Definition{
    OpConstant: {"OpConstant", []int{2}},
}

func Lookup(op byte) (*Definition, error) {
    def, ok := definitions[Opcode(op)]
    if !ok {
        return nil, fmt.Errorf("opcode %d undefined", op)
    }
    return def, nil
}

func Make(op Opcode, operands ...int) []byte {
    def, ok := definitions[op]
    if !ok {return []byte{} }
    instructionLen := 1
    for _, w := range def.OperandWidths {
           instructionLen += w
       }
       instruction := make([]byte, instructionLen)
       instruction[0] = byte(op)
offset := 1
    for i, o := range operands {
        width := def.OperandWidths[i]
        switch width {
            case 2:
                        binary.BigEndian.PutUint16(instruction[offset:], uint16(o))
                    }
                    offset += width
        }
return instruction }

// There should be a make() funciton that create byte sequence.

make(OpCode, operand: Vec<u8>) -> Vec<u8>;
The result here has the Opcode, val

func Make(op Opcode, operands ...int) []byte {
    def, ok := definitions[op]
    if !ok {
    return []byte{} }
    instructionLen := 1
    for _, w := range def.OperandWidths {
            instructionLen += w
    }
        instruction := make([]byte, instructionLen)
        instruction[0] = byte(op)
    offset := 1
    for i, o := range operands {
    width := def.OperandWidths[i]
    switch width {
        case 2:
                    binary.BigEndian.PutUint16(instruction[offset:], uint16(o))
                }
            offset += width
        }
    return instruction
}



   // compiler/compiler.go
func (c *Compiler) Compile(node ast.Node) error {
    switch node := node.(type) {
case *ast.Program:
for _, s := range node.Statements { err := c.Compile(s)
if err != nil {
return err }
}
case *ast.ExpressionStatement:
err := c.Compile(node.Expression) if err != nil {
return err }
41
case *ast.InfixExpression:
err := c.Compile(node.Left) if err != nil {
return err }
err = c.Compile(node.Right) if err != nil {
return err }
case *ast.IntegerLiteral: // TODO: What now?!
}
return nil }


func (c *Compiler) emit(op code.Opcode, operands ...int) int {
    ins := code.Make(op, operands...)
    pos := c.addInstruction(ins)
    return pos
}


func (c *Compiler) addInstruction(ins []byte) int {
     posNewInstruction := len(c.instructions)
     c.instructions = append(c.instructions, ins...)
     return posNewInstruction
}

func (vm *VM) Run() error {
    for ip := 0; ip < len(vm.instructions); ip++ {
        op := code.Opcode(vm.instructions[ip])
        switch op {
            constIndex := code.ReadUint16(vm.instructions[ip+1:])
            ip += 2
        }
    }
    return nil
}

// Example of bytecode repr

0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535


func (ins Instructions) fmtInstruction(def *Definition, operands []int) string {
    // [...]
    switch operandCount {
    case 0:
    return def.Name
    case 1:
    return fmt.Sprintf("%s %d", def.Name, operands[0])
    }
    return fmt.Sprintf("ERROR: unhandled operandCount for %s\n", def.Name)
}

func (ins Instructions) String() string {
    var out bytes.Buffer
    i := 0
    for i < len(ins) {
    def, err := Lookup(ins[i])
    if err != nil {
    fmt.Fprintf(&out, "ERROR: %s\n", err)
    continue
    }
    operands, read := ReadOperands(def, ins[i+1:])
    fmt.Fprintf(&out, "%04d %s\n", i, ins.fmtInstruction(def, operands))
    i += 1 + read
    }
    return out.String()
}

func (ins Instructions) fmtInstruction(def *Definition, operands []int) string {
    operandCount := len(def.OperandWidths)
    if len(operands) != operandCount {
    return fmt.Sprintf("ERROR: operand len %d does not match defined %d\n",
    len(operands), operandCount)
    }
    switch operandCount {
    case 1:
    return fmt.Sprintf("%s %d", def.Name, operands[0])
    }
    return fmt.Sprintf("ERROR: unhandled operandCount for %s\n", def.Name)
}
**/

pub type Bytes = Vec<u8>;

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum OpCodeType {
    OpConstant,
    OpBinAdd,
}

#[derive(Debug)]
pub struct Instruction {
    type_: OpCodeType,
    len: usize,
    data: Bytes,
}

impl Display for OpCodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OpCodeType::*;
        match self {
            OpConstant => write!(f, "OpConstant"),
            OpBinAdd => write!(f, "OpBinAdd"),
        }
    }
}

impl Instruction {
    pub fn new(type_: OpCodeType, len: usize, data: Vec<u8>) -> Result<Self> {
        if !data.len() == len {
            bail!("the len of data is {}, but should be {}", data.len(), len);
        }
        Ok(Self { type_, len, data })
    }

    pub fn make(&self) -> Vec<u8> {
        let mut instruction = vec![];

        instruction.push(self.type_.clone() as u8);
        instruction.extend(self.data.iter());

        instruction
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.make())
    }
}

#[derive(Debug)]
struct Compiler {
    instructions: Vec<u8>,
    constants: Vec<ExprResult>,
}

impl Compiler {
    pub fn add_constant(&mut self, e: ExprResult) -> usize {
        self.constants.push(e);
        self.constants.len() - 1
    }

    pub fn add_instruction(&mut self, i: Instruction) -> usize {
        let pos_new_i = self.instructions.len();
        self.instructions.extend(i.make());
        pos_new_i
    }

    pub fn compile(&mut self, program: Program) -> Result<()> {
        for s in program {
            match s {
                Statement::Expr(e) => match e {
                    Expr::Literal(l) => {
                        if let Literal::UnsignedInteger(val) = l {
                            println!("found a literal");
                            let v_bytes = (val as u64).to_be_bytes().to_vec();
                            self.add_instruction(Instruction::new(
                                OpCodeType::OpConstant,
                                v_bytes.len(),
                                v_bytes,
                            )?);
                        }
                    }
                    _ => {
                        bail!("can't compile expr: {:?}", &e);
                    }
                },
                _ => {
                    bail!("compilation of statement {:?} isn't supported.", &s);
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
struct VM {
    instructions: Bytes,
    constants: Vec<ExprResult>,
    stack: Vec<ExprResult>,
    sp: usize,
}

impl VM {
    pub fn new(instr: Bytes, consts: Vec<ExprResult>) -> Self {
        VM {
            instructions: instr,
            constants: consts,
            stack: vec![],
            sp: 0,
        }
    }

    pub fn run(&mut self) -> Result<()> {
        for i in self.instructions.iter() {
            // use OpCodeType::*;
            // match i.type_ {
            //     OpConstant => {}
            //     OpBinAdd => {}
            // }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_opcode() {
        let OpConstant = Instruction::new(OpCodeType::OpConstant, 2, vec![34, 34]).unwrap();
        println!("{:?}", OpConstant.make());
        println!("Display: {}", OpConstant);
    }
}
