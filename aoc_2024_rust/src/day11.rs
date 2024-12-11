use std::collections::HashMap;

pub fn task1(input:&str) {
    let stones: HashMap<u128, u128> = parse_input(input);
    let end_stones: HashMap<u128, u128> = blink(stones, 25);
    let end_stone_amount: u128 = end_stones.iter().fold(0,|acc, (_, val)| acc + val);
    println!("After blinking 25 times there are {} stones", end_stone_amount);
}

pub fn task2(input:&str) {
    let stones: HashMap<u128, u128> = parse_input(input);
    let end_stones: HashMap<u128, u128> = blink(stones, 75);
    let end_stone_amount: u128 = end_stones.iter().fold(0,|acc, (_, val)| acc + val);
    println!("After blinking 75 times there are {} stones", end_stone_amount);
}

fn blink(stones:HashMap<u128, u128>, blinks:u32) -> HashMap<u128, u128> {
    if blinks == 0 {
        stones
    } else {
        let mut next_stones: HashMap<u128, u128> = HashMap::new();
        for (stone_num, stone_amount) in stones {
            if stone_num == 0 {
                *next_stones.entry(1).or_default() += stone_amount;
            } else {
                let digits: u32 = stone_num.ilog10() + 1;
                if digits % 2 == 0 {
                    let stone1: u128 = stone_num / (10 as u128).pow(digits / 2);
                    let stone2: u128 = stone_num % (10 as u128).pow(digits / 2);
                    *next_stones.entry(stone1).or_default() += stone_amount;
                    *next_stones.entry(stone2).or_default() += stone_amount;
                } else {
                    *next_stones.entry(stone_num * 2024).or_default() += stone_amount
                }
            }
        }
        blink(next_stones, blinks - 1)
    }
}

fn parse_input(input:&str) -> HashMap<u128, u128> {
    let mut stone_map: HashMap<u128, u128> = HashMap::new();
    for num in input.split_whitespace().map(|num| num.parse::<u128>().unwrap()) {
        *stone_map.entry(num).or_default() += 1;
    }
    stone_map
}
