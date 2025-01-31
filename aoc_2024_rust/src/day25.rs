pub fn task1(input:&str) {
    let (lock_vals, key_vals): (Vec<Vec<u8>>, Vec<Vec<u8>>) = parse_input(input);
    let fitting_key_amnt: Vec<u32> = lock_vals.into_iter().map(|lock| get_possible_key_amnt(lock, key_vals.clone())).collect();
    println!("Pairs: {}", fitting_key_amnt.iter().sum::<u32>());
}

pub fn task2(input:&str) {
    println!("TODO: Task 2")
}

fn get_possible_key_amnt(lock:Vec<u8>, keys:Vec<Vec<u8>>) -> u32 {
    keys.into_iter().filter(|key| lock_accepts_key(&lock, key)).count() as u32
}

fn lock_accepts_key(lock:&Vec<u8>, key:&Vec<u8>) -> bool {
    lock.iter().zip(key.iter()).all(|(&l_pin, &k_pin)| l_pin + k_pin <= 5)
}

fn parse_input(input:&str) -> (Vec<Vec<u8>>, Vec<Vec<u8>>) {
    let blocks: Vec<&str> = input.split("\n\n").collect();
    
    let lock_strs: Vec<&str> = blocks.clone().into_iter().filter(|&block| block.split_once("\n").unwrap().0 == "#####").collect();
    let key_strs: Vec<&str> = blocks.into_iter().filter(|&block| block.split_once("\n").unwrap().0 == ".....").collect();

    let lock_vals:Vec<Vec<u8>> = lock_strs.iter().map(|&lock_str| str_to_lock_or_key(lock_str)).collect();
    let key_vals:Vec<Vec<u8>> = key_strs.iter().map(|&key_str| str_to_lock_or_key(key_str)).collect();

    (lock_vals, key_vals)
}

fn str_to_lock_or_key(lk_str:&str) -> Vec<u8> {
    let lk_chars: Vec<Vec<char>> = lk_str.split("\n").map(|row| row.chars().collect()).collect();
    let mut lk_vals: Vec<u8> = Vec::new();
    for col in 0..5 {
        let mut acc: u8 = 0;
        for row in 1..6 {
            if lk_chars[row][col] == '#' {
                acc += 1;
            }
        }
        lk_vals.push(acc);
    }
    lk_vals
}
