pub fn task1(input:&str) {
    let (mut first_list, mut second_list): (Vec<i32>, Vec<i32>) = parse_input(input);
    first_list.sort();
    second_list.sort();
    let total_dist:i32 = first_list.iter().zip(second_list.iter()).map(|(a, b)| (a - b).abs()).sum();
    println!("Total distance is: {}", total_dist);
}

pub fn task2(input:&str) {
    let (first_list, second_list): (Vec<i32>, Vec<i32>) = parse_input(input);
    let similarity_score: i32 = first_list.iter().fold(0, |acc, val| acc + val * (second_list.iter().filter(|elem| elem == &val).count() as i32));
    println!("Similarity score is: {}", similarity_score);
}

fn parse_input(input:&str) -> (Vec<i32>, Vec<i32>){
    input.split("\n").into_iter().map(parse_line).unzip()
}

fn parse_line(line:&str) -> (i32, i32) {
    let (first_str, second_str): (&str, &str) = line.split_once("   ").unwrap();
    (parse_num(first_str), parse_num(second_str))
}

fn parse_num(num_str:&str) -> i32 {
    match num_str.parse::<i32>() {
        Ok(num) => num,
        Err(_) => panic!("Problem parsing number!"),
    }
}
