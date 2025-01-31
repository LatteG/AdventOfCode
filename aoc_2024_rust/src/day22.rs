use std::{cmp::{max, min}, collections::{HashMap, HashSet, VecDeque}, ops::Index, path::absolute};

pub fn task1(input:&str) {
    let initial_secret_numbers: Vec<u64> = parse_input(input);
    let generated_secret_numbers: Vec<u64> = initial_secret_numbers.iter().map(|secret| get_nth_secret_number(*secret, 2000)).collect();

    println!("Sum of all 2000th secret numbers is: {}", generated_secret_numbers.iter().sum::<u64>());
}

pub fn task2(input:&str) {
    let initial_secret_numbers: Vec<u64> = parse_input(input);
    let steps: usize = 2000;
    let secret_number_sequences: Vec<Vec<u64>> = initial_secret_numbers.iter().map(|secret| get_secret_number_sequence(*secret, steps)).collect();
    let mut sequences: HashMap<Vec<i64>, u64> = HashMap::new();
    for sns in secret_number_sequences {
        let mut visited_sequences: HashSet<Vec<i64>> = HashSet::new();
        let prices: Vec<u64> = sns.iter().map(|n| n % 10).collect();
        let price_diffs: Vec<i64> = prices[0..steps].iter().zip(prices[1..].iter()).map(|(&v0, &v1)| v1 as i64 - v0 as i64).collect();
        for i in 4..=steps {
            let seq: Vec<i64> = price_diffs[(i-4)..i].to_vec();
            if !visited_sequences.contains(&seq) {
                visited_sequences.insert(seq.clone());
                sequences.insert(seq.clone(), sequences.get(&seq).unwrap_or(&0) + prices[i]);
            }
        }
    }
    let profit: u64 = *sequences.values().max().unwrap();
    println!("Total bananas: {}", profit);
}

fn parse_input(input:&str) -> Vec<u64> {
    input.lines().map(|line| line.parse::<u64>().unwrap()).collect()
}

fn get_secret_number_sequence(secret:u64, steps:usize) -> Vec<u64> {
    let mut num_seq: Vec<u64> = vec![secret];
    for _ in 0..steps {
        num_seq.push(step_secret_number(*num_seq.last().unwrap()));
    }
    num_seq
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
