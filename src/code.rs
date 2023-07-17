use std::fmt::Display;

use anyhow::{bail, Result};

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

**/

#[derive(Debug)]
enum OpCodeType {
    OpConstant,
}

impl Display for OpCodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OpCodeType::*;
        match self {
            OpConstant => write!(f, "OpConstant"),
        }
    }
}

#[derive(Debug)]
struct OpCode {
    type_: OpCodeType,
    len: usize,
    // TODO: Use slice instead as the size wil be constant.
    data: Vec<u8>,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_)
    }
}

impl OpCode {
    fn new(type_: OpCodeType, len: usize, data: Vec<u8>) -> Result<Self> {
        if !data.len() == len {
            bail!("the len of data is {}, but should be {}", data.len(), len);
        }
        Ok(Self { type_, len, data })
    }
}

// #[derive(Debug)]
// struct Compiler {
//     instructions: Instructio,
//     constants: Vec<u8>,
// }

// impl Compiler {
//     // TODO
// }
