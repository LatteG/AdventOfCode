use std::iter;

type Equation = (i128, Vec<i128>);
type Operator = fn(i128, i128) -> i128;

fn print_add(a:i128, b:i128) -> i128 {
    println!("{} + {} = {}", a, b, a + b);
    a+b
}

fn print_multiply(a:i128, b:i128) -> i128 {
    println!("{} * {} = {}", a, b, a * b);
    a*b
}

fn print_concat(a:i128, b:i128) -> i128 {
    print!("{} || {} = {}", a, b, int_concat(a, b));
    int_concat(a, b)
}

fn int_concat(a:i128, b:i128) -> i128 {
    a * (10 as i128).pow(b.checked_ilog10().unwrap_or(0) + 1) + b
}

pub fn task1(input:&str) {
    let equations: Vec<Equation> = parse_input(input);
    let operators: Vec<Operator> = vec![|a, b| a + b, |a, b| a * b];
    let calibration_equations: Vec<Equation> = equations.into_iter().filter(|eq| check_equation(eq.clone(), operators.clone())).collect();
    let calibration_result: i128 = calibration_equations.iter().fold(0, |acc, (val, _)| acc + val);
    println!("Calibration result: {}", calibration_result);
}

pub fn task2(input:&str) {
    let equations: Vec<Equation> = parse_input(input);
    let operators: Vec<Operator> = vec![|a, b| a + b, |a, b| a * b, int_concat];
    let calibration_equations: Vec<Equation> = equations.into_iter().filter(|eq| check_equation(eq.clone(), operators.clone())).collect();
    let calibration_result: i128 = calibration_equations.iter().fold(0, |acc, (val, _)| acc + val);
    println!("Calibration result: {}", calibration_result);
}

fn check_equation(equation:Equation, operators:Vec<Operator>) -> bool {
    if equation.1.len() == 1 {
        equation.0 == equation.1[0]
    } else if equation.0 < equation.1[0] {
        false
    } else {
        operators.iter().any(|op| step_check_equation(equation.clone(), operators.clone(), *op))
    }
}

fn step_check_equation(equation:Equation, operators:Vec<Operator>, operator:Operator) -> bool {
    let head: i128 = operator(equation.1[0], equation.1[1]);
    let new_parts: Vec<i128> = [vec![head], equation.1[2..].to_vec()].concat();
    check_equation((equation.0, new_parts), operators)
}

// fn check_equation(equation:Equation, inverted_operators:Vec<Operator>) -> bool {
//     if equation.1.len() == 1 {
//         equation.0 == equation.1[0]
//     } else if equation.0 < 0{
//         false
//     } else {
//         inverted_operators.iter().any(|op| check_equation((op(equation.0, *equation.1.last().unwrap()), equation.1[..equation.1.len() - 1].to_vec()), inverted_operators.clone()))
//     }    
// }

fn parse_input(input:&str) -> Vec<Equation> {
    let lines:Vec<&str> = input.split("\n").collect();
    lines.into_iter().map(parse_line).collect()
}

fn parse_line(line:&str) -> Equation {
    let (sum_str, parts_str): (&str, &str) = line.split_once(": ").unwrap();
    let sum: i128 = parse_num(sum_str);
    let parts: Vec<i128> = parts_str.split_whitespace().map(parse_num).collect();
    (sum, parts)
}

fn parse_num(num_str:&str) -> i128 {
    match num_str.parse::<i128>() {
        Ok(num) => num,
        Err(_) => panic!("Problem parsing number: {}", num_str),
    }
}
