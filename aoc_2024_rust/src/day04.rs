use std::ops::Index;

use regex::Regex;

pub fn task1(input:&str) {
    let wordsearch: Vec<&str> = parse_input(input);
    let re: Regex = Regex::new(r"(?:XMAS|SMAX)").unwrap();
    let xmas_count: u32 = count_all_xmas(wordsearch, re);
    println!("Found {} XMAS", xmas_count);
}

pub fn task2(input:&str) {
    println!("TODO: Task 2")
}

fn count_all_xmas(wordsearch:Vec<&str>, re:Regex) -> u32 {
    let mut count: u32 = 0;

    count += count_xmas(&wordsearch, &re);

    let binding = rotate_90_deg(&wordsearch);
    let ws_90_deg:Vec<&str> = binding.iter().map(|row| row.as_str()).collect();

    count += count_xmas(&ws_90_deg, &re);

    count
}

fn rotate_90_deg(wordsearch: &Vec<&str>) -> Vec<String> {
    let mut ws_90_deg:Vec<String> = Vec::new();
    for y in 0..wordsearch[0].len() {
        let mut row: Vec<char> = Vec::new();
        for x in 0..wordsearch.len() {
            row.push(wordsearch[x].chars().collect::<Vec<char>>()[y]);
        }
        ws_90_deg.push(row.iter().collect::<String>())
    }
    ws_90_deg
}

fn rotate_45_deg(wordsearch: &Vec<&str>) -> Vec<String> {
    let mut ws_90_deg:Vec<String> = Vec::new();
    for y in 0..wordsearch[0].len() {
        let mut row: Vec<char> = Vec::new();
        for x in 0..wordsearch.len() {
            row.push(wordsearch[x].chars().collect::<Vec<char>>()[y]);
        }
        ws_90_deg.push(row.iter().collect::<String>())
    }
    ws_90_deg
}

fn count_xmas(wordsearch:&Vec<&str>, re:&Regex) -> u32 {
    wordsearch.iter().fold(0, |acc, line:&&str| acc + re.find_iter(&line).count() as u32)
}

fn parse_input(input:&str) -> Vec<&str> {
    input.split("\n").collect()
}
