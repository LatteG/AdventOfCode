pub fn task1(input:&str) {
    let reports: Vec<Vec<i32>> = parse_input(input);
    let safe_report_amount: usize = reports.iter().filter(report_is_safe).count();

    println!("There are {} safe reports", safe_report_amount)
}

pub fn task2(input:&str) {
    let reports: Vec<Vec<i32>> = parse_input(input);
    let safe_report_amount: usize = reports.iter().filter(report_is_somewhat_safe).count();

    println!("There are {} safe reports", safe_report_amount)
}

fn report_is_safe(report:&&Vec<i32>) -> bool {
    let delta_values: Vec<i32> = report[0..(report.len()-1)].iter().zip(report[1..].iter()).map(|(a, b)|a-b).collect();
    let increasing: bool = delta_values.iter().all(|val|val >= &1 && val <= &3);
    let decreasing: bool = delta_values.iter().all(|val|val <= &-1 && val >= &-3);
    increasing || decreasing
}

fn report_is_somewhat_safe(report:&&Vec<i32>) -> bool {
    if report_is_safe(report) {
        return true;
    }

    for i in 0..(report.len()) {
        let rep: Vec<i32> = [&report[..i], &report[(i + 1)..]].concat();
        if report_is_safe(&&rep) {
            return true;
        }
    }

    return false;
}

fn parse_input(input:&str) -> Vec<Vec<i32>> {
    let lines: Vec<&str> = input.split("\n").collect();
    lines.into_iter().map(parse_line).collect()
}

fn parse_line(line:&str) -> Vec<i32> {
    let numbers: Vec<&str> = line.split_whitespace().collect();
    numbers.into_iter().map(parse_num).collect()
}

fn parse_num(num_str:&str) -> i32 {
    match num_str.parse::<i32>() {
        Ok(num) => num,
        Err(_) => panic!("Problem parsing number"),
    }
}
