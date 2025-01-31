#![allow(unused)]

use std::{fs, io};
use std::collections::HashMap;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day20;
mod day22;
mod day24;
mod day25;

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
    days.insert(  1, |s|{day01::task1(s); day01::task2(s);});
    days.insert(  2, |s|{day02::task1(s); day02::task2(s);});
    days.insert(  3, |s|{day03::task1(s); day03::task2(s);});
    days.insert(  4, |s|{day04::task1(s); day04::task2(s);});
    days.insert(  5, |s|{day05::task1(s); day05::task2(s);});
    days.insert(  6, |s|{day06::task1(s); day06::task2(s);});
    days.insert(  7, |s|{day07::task1(s); day07::task2(s);});
    days.insert(  8, |s|{day08::task1(s); day08::task2(s);});
    days.insert(  9, |s|{day09::task1(s); day09::task2(s);});
    days.insert( 10, |s|{day10::task1(s); day10::task2(s);});
    days.insert( 11, |s|{day11::task1(s); day11::task2(s);});
    days.insert( 12, |s|{day12::task1(s); day12::task2(s);});
    days.insert( 13, |s|{day13::task1(s); day13::task2(s);});
    days.insert( 14, |s|{day14::task1(s); day14::task2(s);});
    days.insert( 15, |s|{day15::task1(s); day15::task2(s);});
    days.insert( 16, |s|{day16::task1(s); day16::task2(s);});
    days.insert( 17, |s|{day17::task1(s); day17::task2(s);});
    days.insert( 18, |s|{day18::task1(s); day18::task2(s);});
    days.insert( 19, |s|{day19::task1(s); day19::task2(s);});
    days.insert( 20, |s|{day20::task1(s); day20::task2(s);});
    days.insert( 22, |s|{day22::task1(s); day22::task2(s);});
    days.insert( 24, |s|{day24::task1(s); day24::task2(s);});
    days.insert( 25, |s|{day25::task1(s); day25::task2(s);});

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
