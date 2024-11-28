use std::collections::VecDeque;

struct ScratchCard {
    id: i32,
    winning_numbers: Vec<i32>,
    numbers: Vec<i32>,
}

impl ScratchCard {
    fn get_points(&self) -> i32 {
        let matches:i32 = self.get_match_count();
        if matches == 0 {
            0
        } else {
            (2 as i32).pow(matches as u32 - 1)
        }
    }
    fn get_match_count(&self) -> i32 {
        let mut matches:i32 = 0;
        for n in self.numbers.as_slice() {
            if self.winning_numbers.iter().any(|wn| wn == n) {
                matches += 1;
            }
        }
        return matches;
    }
}

pub fn task1(input:&str) {
    let cards:Vec<ScratchCard> = parse_input(input);
    let points:Vec<i32> = cards.iter().map(|c| c.get_points()).collect();
    println!("Points: {}", points.iter().sum::<i32>());
}

pub fn task2(input:&str) {
    let cards:Vec<ScratchCard> = parse_input(input);
    let total_card_count:i32 = get_total_card_count(cards);
    println!("Total card count: {}", total_card_count);
}

fn parse_input(input:&str) -> Vec<ScratchCard> {
    let card_strings:Vec<&str> = input.split("\n").collect();
    card_strings.iter().map(|cs| parse_card(cs)).collect()
}

fn parse_card(card_string:&str) -> ScratchCard {
    let (id_string, card_content_string):(&str, &str) = card_string.split_once(": ").unwrap();
    let id:i32 = match id_string.split_once(" ").unwrap().1.trim().parse::<i32>() {
        Ok(i) => i,
        Err(..) => panic!("Problem parsing card ID!"),
    };
    let (win_num_str, num_str):(&str, &str) = card_content_string.split_once(" | ").unwrap();
    let winning_numbers:Vec<i32> = parse_numbers(win_num_str);
    let _numbers:Vec<i32> = parse_numbers(num_str);

    ScratchCard {
        id: id,
        winning_numbers: winning_numbers,
        numbers: _numbers,
    }
}

fn parse_numbers(numbers_str:&str) -> Vec<i32> {
    numbers_str.split_whitespace().map(|n| match n.parse::<i32>() {
        Ok(i) => i,
        Err(..) => panic!("Problem parsing numbers!")
    }).collect()
}

fn get_total_card_count(cards:Vec<ScratchCard>) -> i32 {
    let mut card_count:i32 = 0;
    let mut win_counters:VecDeque<i32> = VecDeque::new();

    for card in cards.as_slice() {
        let copies:i32 = match win_counters.pop_front() {
            None => 0,
            Some(i) => i,
        } + 1;
        card_count += copies;
        let wins:i32 = card.get_match_count();
        for i in 0..(wins as usize) {
            if i < win_counters.len() {
                win_counters[i] += copies;
            } else {
                win_counters.push_back(copies);
            }
        }
        //println!("Card {}:\n\tCopies: {}\n\tCard count: {}\n\tCounters: {:?}\n", card.id, copies, card_count, win_counters);
    }
    return card_count;
}
