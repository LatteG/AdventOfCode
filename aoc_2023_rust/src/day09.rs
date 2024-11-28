pub fn task1(input:&str) {
    let histories:Vec<Vec<i32>> = parse_input(input);
    let predicted_vals:Vec<i32> = histories.iter().map(|history| predict_next_val(history.to_vec())).collect();
    println!("Sum of predicted next values: {}", predicted_vals.iter().sum::<i32>());
}

pub fn task2(input:&str) {
    let histories:Vec<Vec<i32>> = parse_input(input);
    let predicted_vals:Vec<i32> = histories.iter().map(|history| predict_previous_val(history.to_vec())).collect();
    println!("Sum of predicted previous values: {}", predicted_vals.iter().sum::<i32>());
}

fn parse_input(input:&str) -> Vec<Vec<i32>> {
    let lines:Vec<&str> = input.split("\n").collect();
    lines.iter().map(|l| parse_line(l)).collect()
}

fn parse_line(line:&str) -> Vec<i32> {
    line.split_whitespace().map(|num_str| match num_str.parse::<i32>() {
        Ok(num) => num,
        Err(..) => panic!("Problem parsing number!"),
    }).collect()
}

fn predict_previous_val(history:Vec<i32>) -> i32 {
    predict_next_val(history.iter().copied().rev().collect())
}

fn predict_next_val(history:Vec<i32>) -> i32 {
    if history.iter().all(|num| num == &0) {
        return 0;
    }

    let mut derivative:Vec<i32> = Vec::new();
    for i in 1..history.len() {
        derivative.push(history[i] - history[i-1]);
    }
    let next_derivative:i32 = predict_next_val(derivative);
    history.last().unwrap() + next_derivative
}