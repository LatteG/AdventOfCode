#![allow(unused)]

use std::{fs, io};
use std::collections::HashMap;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;

fn main() {
    println!("Which day should we check?");
    let mut day: String = String::new();
    io::stdin().read_line(&mut day).expect("Failed to read line");

    match day.trim().parse::<u8>() {
        Ok(day_num) => {
            println!("Let's check day{:02}", day_num);
            run_day(day_num);
        },
        Err(..) => println!("This was not an integer"),
    };
}

fn run_day(day_num:u8) {
    let mut days = HashMap::<u8, fn(&str)>::new();
    days.insert( 1, |s|{day01::task1(s); day01::task2(s);});
    days.insert( 2, |s|{day02::task1(s); day02::task2(s);});
    days.insert( 3, |s|{day03::task1(s); day03::task2(s);});
    days.insert( 4, |s|{day04::task1(s); day04::task2(s);});
    days.insert( 5, |s|{day05::task1(s); day05::task2(s);});

    let day_funs = days.get(&day_num).unwrap();

    let input:&str = &read_input(day_num);
    day_funs(input.trim());
}

fn read_input(day_num:u8) -> String {
    println!("Is this a real run? (y/n)");
    let mut is_real:String = String::new();
    io::stdin().read_line(&mut is_real).expect("Failed to read line");

    let test_str:&str = match is_real.trim() {
        "y" => "",
        _   => "_test",
    };

    let path_str:String = format!("./input/day{:02}{}.txt", day_num, test_str);
    match fs::read_to_string(path_str) {
        Ok(input) => input,
        Err(..) => panic!("Could not find input file"),
    }
}
