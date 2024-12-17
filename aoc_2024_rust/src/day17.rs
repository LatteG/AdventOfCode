#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Register {
    a: u64,
    b: u64,
    c: u64
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Op {
    ADV(u32),
    BXL(u32),
    BST(u32),
    JNZ(u32),
    BXC(u32),
    OUT(u32),
    BDV(u32),
    CDV(u32)
}

type Program = Vec<Op>;

pub fn task1(input:&str) {
    let (register, program): (Register, Program) = parse_input(input);
    let program_output: Vec<u32> = evaluate_program(register, program);
    println!("Program output: {}", program_output.iter().map(|num| num.to_string()).collect::<Vec<String>>().join(","));
}

pub fn task2(input:&str) {
    let (register, program): (Register, Program) = parse_input(input);
    println!("Program: {:?}", program);
    let program_nums: Vec<u32> = input.split_once("Program: ").unwrap().1.split(",").map(|num_str| num_str.parse::<u32>().unwrap()).collect();
    let mut output_program: Program = Vec::new();
    let mut a_start_value: u64 = 1;
    loop {
        let output: Vec<u32> = evaluate_program(Register { a: a_start_value, b: 0, c: 0 }, program.clone());
        let mut matches: u32 = 0;
        for i in 0..output.len() {
            if output[i] == program_nums[i] {
                matches += 1;
            } else {
                break;
            }
        }
        if matches == program_nums.len() as u32 {
            break;
        } else {
            a_start_value += 8_u64.pow(matches);
        }
    }

    println!("Program loops when a = {}", a_start_value);
}

fn evaluate_program(mut register:Register, program:Program) -> Vec<u32> {
    let mut output: Vec<u32> = Vec::new();
    let mut index: usize = 0;

    while index < program.len() {
        match program[index] {
            Op::ADV(num) => {
                let temp: u64 = register.a / 2_u64.pow(evaluate_combo_operand(num, register));
                register = Register { a: temp, b: register.b, c: register.c };
                index += 1;
            },
            Op::BXL(num) => {
                let temp: u64 = register.b ^ num as u64;
                register = Register { a: register.a, b: temp, c: register.c };
                index += 1;
            },
            Op::BST(num) => {
                let temp: u64 = evaluate_combo_operand(num, register) as u64 % 8;
                register = Register { a: register.a, b: temp, c: register.c };
                index += 1;
            },
            Op::JNZ(num) => {
                if register.a != 0 {
                    index = (num / 2) as usize;
                } else {
                    index += 1;
                }
            },
            Op::BXC(_) => {
                let temp: u64 = register.b ^ register.c;
                register = Register { a: register.a, b: temp, c: register.c };
                index += 1;
            },
            Op::OUT(num) => {
                let temp: u32 = evaluate_combo_operand(num, register) % 8;
                output.push(temp);
                index += 1;
            },
            Op::BDV(num) => {
                let temp: u64 = register.a / 2_u64.pow(evaluate_combo_operand(num, register));
                register = Register { a: register.a, b: temp, c: register.c };
                index += 1;
            }
            Op::CDV(num) => {
                let temp: u64 = register.a / 2_u64.pow(evaluate_combo_operand(num, register));
                register = Register { a: register.a, b: register.b, c: temp };
                index += 1;
            }
        }
    }

    output
}

fn evaluate_combo_operand(operand:u32, register:Register) -> u32 {
    match operand {
        0..=3 => operand as u32,
        4 => register.a as u32,
        5 => register.b as u32,
        6 => register.c as u32,
        _ => panic!("Invalid combo operand!"),
    }
}

fn parse_input(input:&str) -> (Register, Program) {
    let (register_str, program_str): (&str, &str) = input.split_once("\n\n").unwrap();
    let registers: Vec<u32> = register_str.lines().map(|line| line.split_once(": ").unwrap().1.parse::<u32>().unwrap()).collect();
    let program: Program = parse_program(program_str.split_once(": ").unwrap().1);
    (Register{a: registers[0] as u64, b: registers[1] as u64, c: registers[2] as u64}, program)
}

fn parse_program(program_str:&str) -> Program {
    let program_values: Vec<u32> = program_str.split(",").map(|num_str| num_str.parse::<u32>().unwrap()).collect();
    let mut program: Program = Vec::new();
    for i in (0..program_values.len()).step_by(2) {
        match program_values[i] {
            0 => program.push(Op::ADV(program_values[i + 1])),
            1 => program.push(Op::BXL(program_values[i + 1])),
            2 => program.push(Op::BST(program_values[i + 1])),
            3 => program.push(Op::JNZ(program_values[i + 1])),
            4 => program.push(Op::BXC(program_values[i + 1])),
            5 => program.push(Op::OUT(program_values[i + 1])),
            6 => program.push(Op::BDV(program_values[i + 1])),
            7 => program.push(Op::CDV(program_values[i + 1])),
            _ => panic!("Unknown operation: {}", program_values[i]),
        }
    }
    program
}
