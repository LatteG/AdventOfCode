use std::str::FromStr;

use regex::Regex;

pub fn task1(input:&str) {
    let (stock_regex, wanted_patterns): (Regex, Vec<&str>) = parse_input(input);
    let possible_patterns: Vec<&str> = wanted_patterns.into_iter().filter(|pattern| stock_regex.is_match(*pattern)).collect();
    println!("There are {} possible patterns", possible_patterns.len());
}

pub fn task2(input:&str) {
    let stock: Vec<&str> = input.split_once("\n\n").unwrap().0.split(", ").collect();
    let (stock_regex, wanted_patterns): (Regex, Vec<&str>) = parse_input(input);
    let possible_patterns: Vec<&str> = wanted_patterns.into_iter().filter(|pattern| stock_regex.is_match(*pattern)).collect();
    let towel_combinations: u32 = possible_patterns.into_iter().fold(0, |acc, pattern| acc + get_towel_combination_count(pattern, &stock));
    println!("The patterns can be built in {:?} combinations", towel_combinations);
}

fn get_towel_combination_count(pattern: &str, stock: &Vec<&str>) -> u32 {
    if pattern.len() == 0 {
        1
    } else {
        stock.iter().fold(0, |acc, towel| if let Some(remaining_pattern) = pattern.strip_prefix(towel) {
                acc + get_towel_combination_count(remaining_pattern, stock)
            } else {
                acc
            })
    }
}

fn parse_input(input:&str) -> (Regex, Vec<&str>) {
    let (stock_str, wanted_patterns_str): (&str, &str) = input.split_once("\n\n").unwrap();

    let stock_regex_str: String = "^(".to_string() + stock_str.replace(", ", "|").as_str() + ")+$";
    let stock_regex: Regex = Regex::from_str(stock_regex_str.as_str()).unwrap();

    (stock_regex, wanted_patterns_str.lines().collect())
}
