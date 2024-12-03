use regex::Regex;

pub fn task1(input:&str) {
    let mul_pairs:Vec<(i32, i32)> = parse_input(input);
    let sum:i32 = mul_pairs.iter().fold(0, |acc, (a, b)| acc + (a * b));
    println!("The sum is: {}", sum)
}

pub fn task2(input:&str) {
    let mul_pairs:Vec<(i32, i32)> = parse_input_2(input);
    let sum:i32 = mul_pairs.iter().fold(0, |acc, (a, b)| acc + (a * b));
    println!("The sum is: {}", sum)
}

fn parse_input_2(input:&str) -> Vec<(i32,i32)> {
    let re: Regex = Regex::new(r"(?:mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\))").unwrap();
    let mut is_enabled:bool = true;
    let mut return_vec:Vec<(i32,i32)> = Vec::new();
    for op_match in re.find_iter(input) {
        let op_str:&str = op_match.as_str();
        if op_str == "do()" {
            is_enabled = true;
        } else if op_str == "don't()" {
            is_enabled = false;
        } else if is_enabled {
            return_vec.push(parse_mul(op_str));
        }
    }
    return_vec
}

fn parse_input(input:&str) -> Vec<(i32, i32)> {
    let re: Regex = Regex::new(r"mul\(\d{1,3},\d{1,3}\)").unwrap();
    let mul_strs: Vec<&str> = re.find_iter(input).map(|m| m.as_str()).collect();
    mul_strs.into_iter().map(parse_mul).collect()
}

fn parse_mul(mul_str:&str) -> (i32, i32) {
    let nums_str = mul_str.replace("mul(", "").replace(")", "");
    let (num_0_str, num_1_str): (&str, &str) = nums_str.split_once(",").unwrap();
    (parse_num(num_0_str), parse_num(num_1_str))
}

fn parse_num(num_str:&str) -> i32 {
    match num_str.parse::<i32>() {
        Ok(num) => num,
        Err(_) => panic!("Problem parsing number"),
    }
}
