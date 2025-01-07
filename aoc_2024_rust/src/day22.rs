use std::{cmp::{max, min}, collections::VecDeque, ops::Index};

pub fn task1(input:&str) {
    let initial_secret_numbers: Vec<u64> = parse_input(input);
    let generated_secret_numbers: Vec<u64> = initial_secret_numbers.iter().map(|secret| get_nth_secret_number(*secret, 2000)).collect();
    
    // for index in 0..initial_secret_numbers.len() {
    //     println!("{}: {}", initial_secret_numbers[index], generated_secret_numbers[index]);
    // }

    println!("Sum of all 2000th secret numbers is: {}", generated_secret_numbers.iter().sum::<u64>());
    // let mix_test: u64 = mix(15, 42);
    // println!("Test mix 15 & 42, should be 37: {}, {}", mix_test, mix_test == 37);
    // let prune_test: u64 = prune(100000000);
    // println!("Test prune 100000000, should be 16113920: {}, {}", prune_test, prune_test == 16113920);
    
    // let mut secret: u64 = 123;
    // let correct: Vec<u64> = vec![15887950, 16495136, 527345, 704524, 1553684, 12683156, 11100544, 12249484, 7753432, 5908254];
    // for step in 0..10 {
    //     secret = step_secret_number(secret);
    //     println!("{}: {}, {}", step, secret, correct[step]);
    // }
}

pub fn task2(input:&str) {
    let initial_secret_numbers: Vec<u64> = parse_input(input);
    let bananas_gained: u64 = find_best_hiding_spot_sequence(initial_secret_numbers, 2000);
    // let bananas_gained: Vec<u64> = initial_secret_numbers.iter().map(|secret| sell_hiding_spot(vec![-2,1,-1,3], *secret, 2000)).collect();

    // for index in 0..initial_secret_numbers.len() {
    //     println!("{}: {}", initial_secret_numbers[index], bananas_gained[index]);
    // }
    println!("Total bananas: {}", bananas_gained);
}

fn parse_input(input:&str) -> Vec<u64> {
    input.lines().map(|line| line.parse::<u64>().unwrap()).collect()
}

fn find_best_hiding_spot_sequence(initial_secret_numbers:Vec<u64>, max_steps:u64) -> u64 {
    let mut highest_banana_amount: u64 = 0;
    for pattern_1 in -9..=9 {
        let min_step_after_1: i64 = max(-9, -9 - pattern_1);
        let max_step_after_1: i64 = min(9, 9 - pattern_1);
        for pattern_2 in min_step_after_1..=max_step_after_1 {
            let min_step_after_2: i64 = max(-9, -9 - (pattern_1 + pattern_2));
            let max_step_after_2: i64 = min(9, 9 - (pattern_1 + pattern_2));
            for pattern_3 in min_step_after_2..=max_step_after_2 {
                let min_step_after_3: i64 = max(0, -9 - (pattern_1 + pattern_2 + pattern_3));
                let max_step_after_3: i64 = min(9, 9 - (pattern_1 + pattern_2 + pattern_3));
                for pattern_4 in min_step_after_3..=max_step_after_3 {
                    let banana_amount: u64 = initial_secret_numbers.iter().map(|secret| sell_hiding_spot(vec![pattern_1, pattern_2, pattern_3, pattern_4], *secret, max_steps)).sum::<u64>();
                    highest_banana_amount = max(banana_amount, highest_banana_amount);
                }
            }
        }
    }
    highest_banana_amount
}

fn sell_hiding_spot(pattern:Vec<i64>, start:u64, max_steps:u64) -> u64 {
    let mut prev_secret: u64 = start;
    let mut buffer: VecDeque<i64> = VecDeque::new();

    for _ in 0..max_steps {
        let next_secret: u64 = step_secret_number(prev_secret);
        buffer.push_back(next_secret as i64 % 10 - prev_secret as i64 % 10);
        if buffer.len() > 4 {
            buffer.pop_front();
            if is_pattern(&pattern, &buffer) {
                return next_secret % 10;
            }
        }
        prev_secret = next_secret;
    }
    0
}

fn is_pattern(pattern:&Vec<i64>, buffer:&VecDeque<i64>) -> bool {
    for index in 0..pattern.len() {
        if pattern[index] != buffer[index] {
            return false;
        }
    }
    true
}

fn get_nth_secret_number(secret:u64, steps:u64) -> u64 {
    let mut temp: u64 = secret;
    for _ in 0..steps {
        temp = step_secret_number(temp);
    }
    temp
}

fn step_secret_number(old_secret:u64) -> u64 {
    let step_1: u64 = prune(mix(old_secret * 64, old_secret));
    let step_2: u64 = prune(mix(step_1 / 32, step_1));
    prune(mix(step_2 * 2048, step_2))
}

fn mix(val:u64, secret:u64) -> u64 {
    val ^ secret
}

fn prune(secret:u64) -> u64 {
    secret % 2_u64.pow(24)
}
