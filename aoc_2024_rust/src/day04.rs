use std::ops::Index;

use regex::Regex;

type WordSearch = Vec<Vec<char>>;

pub fn task1(input:&str) {
    let wordsearch: WordSearch = parse_input(input);
    let re: (Regex, Regex) = (Regex::new(r"XMAS").unwrap(), Regex::new(r"SAMX").unwrap());
    let xmas_count: u32 = count_all_xmas(wordsearch, re);
    println!("Found {} XMAS", xmas_count);
}

pub fn task2(input:&str) {
    let wordsearch: WordSearch = parse_input(input);
    let x_mas_count: u32 = count_all_x_mas(wordsearch);
    println!("Found {} X-MAS", x_mas_count);
}

fn count_all_x_mas(wordsearch:WordSearch) -> u32 {
    let mut count: u32 = 0;

    for y in 1..(wordsearch.len() - 1) {
        for x in 1..(wordsearch[0].len() - 1) {
            if wordsearch[y][x] == 'A' {
                if ms_check(wordsearch[y-1][x-1], wordsearch[y+1][x+1]) && ms_check(wordsearch[y+1][x-1], wordsearch[y-1][x+1]) {
                    count += 1;
                }
            }
        }
    }

    count
}

fn ms_check(c_0: char, c_1: char) -> bool {
    ['M', 'S'].contains(&c_0) && ['M', 'S'].contains(&c_1) && c_0 != c_1
}

fn count_all_xmas(wordsearch:WordSearch, re:(Regex, Regex)) -> u32 {
    let mut count: u32 = 0;

    count += count_xmas(&wordsearch, &re);
    
    let ws_45_deg:WordSearch = rotate_45_deg(&wordsearch);
    count += count_xmas(&ws_45_deg, &re);
    
    let ws_90_deg:WordSearch = rotate_90_deg(&wordsearch);
    count += count_xmas(&ws_90_deg, &re);
    
    let ws_135_deg:WordSearch = rotate_45_deg(&ws_90_deg);
    count += count_xmas(&ws_135_deg, &re);

    count
}

fn rotate_90_deg(wordsearch: &WordSearch) -> WordSearch {
    let mut ws_90_deg:WordSearch = Vec::new();
    for y in 0..wordsearch[0].len() {
        let mut row: Vec<char> = Vec::new();
        for x in (0..wordsearch.len()).rev() {
            row.push(wordsearch[x][y]);
        }
        ws_90_deg.push(row)
    }
    ws_90_deg
}

fn rotate_45_deg(wordsearch: &WordSearch) -> WordSearch {
    let mut rotate_45_deg:WordSearch = Vec::new();
    for dist in 1..(wordsearch[0].len() +  wordsearch.len()) {
        let mut row: Vec<char> = Vec::new();
        for x in 0..dist {
            let y: usize = dist - 1- x;
            // println!("({},{})", x, y);
            if x < wordsearch[0].len() && y < wordsearch.len() {
                row.push(wordsearch[y][x]);
            }
        }
        rotate_45_deg.push(row)
    }
    rotate_45_deg
}

fn count_xmas(wordsearch:&WordSearch, re:&(Regex, Regex)) -> u32 {
    let (re0, re1): &(Regex, Regex) = re;
    let lines: Vec<String> = wordsearch.iter().map(|line| line.iter().collect()).collect();
    lines.iter().fold(0, |acc, line| acc + re0.find_iter(&line).count() as u32 + re1.find_iter(&line).count() as u32)
}

fn parse_input(input:&str) -> WordSearch {
    input.split("\n").map(|row| row.chars().collect()).collect()
}
