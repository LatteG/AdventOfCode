use std::{collections::HashMap, fmt::Debug, hash::Hash};

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Copy, Clone)]
enum CardType {
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    T,
    J,
    Q,
    K,
    A,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Copy, Clone)]
enum CardTypeWithJoker {
    J,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    T,
    Q,
    K,
    A,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Copy, Clone, Debug)]
enum HandType {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

#[derive(PartialEq, PartialOrd, Eq, Debug)]
struct Hand<T> where T:PartialEq, T:PartialOrd, T:Eq, T:Ord, T:Hash, T:Copy, T:Clone {
    hand_type: HandType,
    cards: Vec<T>,
    bid: u32,
}

impl Ord for Hand<CardType> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.hand_type.cmp(&other.hand_type) {
            std::cmp::Ordering::Equal => {
                self.cards.iter().zip(other.cards.iter()).fold(std::cmp::Ordering::Equal, |ord, (s, o)| {
                    match ord {
                        std::cmp::Ordering::Equal => s.cmp(o),
                        ordering => ordering,
                    }
                })
            }
            ordering => ordering,
        }
    }
}
impl Ord for Hand<CardTypeWithJoker> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.hand_type.cmp(&other.hand_type) {
            std::cmp::Ordering::Equal => {
                self.cards.iter().zip(other.cards.iter()).fold(std::cmp::Ordering::Equal, |ord, (s, o)| {
                    match ord {
                        std::cmp::Ordering::Equal => s.cmp(o),
                        ordering => ordering,
                    }
                })
            }
            ordering => ordering,
        }
    }
}

impl Debug for CardType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Two   => write!(f, "2"),
            Self::Three => write!(f, "3"),
            Self::Four  => write!(f, "4"),
            Self::Five  => write!(f, "5"),
            Self::Six   => write!(f, "6"),
            Self::Seven => write!(f, "7"),
            Self::Eight => write!(f, "8"),
            Self::Nine  => write!(f, "9"),
            Self::T     => write!(f, "T"),
            Self::J     => write!(f, "J"),
            Self::Q     => write!(f, "Q"),
            Self::K     => write!(f, "K"),
            Self::A     => write!(f, "A"),
        }
    }
}

pub fn task1(input:&str) {
    let mut hands:Vec<Hand<CardType>> = parse_input(input);
    hands.sort();
    let score:u32 = get_score(&hands);
    println!("Score with jacks: {}", score);
}

pub fn task2(input:&str) {
    let mut hands:Vec<Hand<CardTypeWithJoker>> = parse_input_with_joker(input);
    hands.sort();
    let score:u32 = get_score(&hands);
    println!("Score with jokers: {}", score);
}

fn parse_input(input:&str) -> Vec<Hand<CardType>> {
    input.split("\n").map(|hand_str| parse_hand(hand_str)).collect()
}

fn parse_input_with_joker(input:&str) -> Vec<Hand<CardTypeWithJoker>> {
    input.split("\n").map(|hand_str| parse_hand_with_joker(hand_str)).collect()
}

fn parse_hand(hand_str:&str) -> Hand<CardType> {
    let (cards_str, bid_str):(&str, &str) = hand_str.split_once(" ").unwrap();

    let bid:u32 = match bid_str.parse::<u32>() {
        Ok(b) => b,
        Err(..) => panic!("Problem parsing bid!"),
    };

    let cards:Vec<CardType> = cards_str.chars().map(|c| char_to_card_type(c)).collect();
    let hand_type:HandType = get_hand_type(&cards);

    Hand{
        hand_type: hand_type,
        cards: cards,
        bid: bid,
    }
}

fn parse_hand_with_joker(hand_str:&str) -> Hand<CardTypeWithJoker> {
    let (cards_str, bid_str):(&str, &str) = hand_str.split_once(" ").unwrap();

    let bid:u32 = match bid_str.parse::<u32>() {
        Ok(b) => b,
        Err(..) => panic!("Problem parsing bid!"),
    };

    let cards:Vec<CardTypeWithJoker> = cards_str.chars().map(|c| char_to_card_type_with_joker(c)).collect();
    let cards_without_jokers:Vec<CardTypeWithJoker>  =  match resolve_jokers(&cards) {
        Some(v) => v,
        None => cards.clone()
    };
    let hand_type:HandType = get_hand_type(&cards_without_jokers);

    Hand{
        hand_type: hand_type,
        cards: cards,
        bid: bid,
    }
}

fn char_to_card_type(c:char) -> CardType {
    match c {
        '2' => CardType::Two,
        '3' => CardType::Three,
        '4' => CardType::Four,
        '5' => CardType::Five,
        '6' => CardType::Six,
        '7' => CardType::Seven,
        '8' => CardType::Eight,
        '9' => CardType::Nine,
        'T' => CardType::T,
        'J' => CardType::J,
        'Q' => CardType::Q,
        'K' => CardType::K,
        'A' => CardType::A,
        _ => panic!("Problem parsing CardType!"),
    }
}
fn char_to_card_type_with_joker(c:char) -> CardTypeWithJoker {
    match c {
        'J' => CardTypeWithJoker::J,
        '2' => CardTypeWithJoker::Two,
        '3' => CardTypeWithJoker::Three,
        '4' => CardTypeWithJoker::Four,
        '5' => CardTypeWithJoker::Five,
        '6' => CardTypeWithJoker::Six,
        '7' => CardTypeWithJoker::Seven,
        '8' => CardTypeWithJoker::Eight,
        '9' => CardTypeWithJoker::Nine,
        'T' => CardTypeWithJoker::T,
        'Q' => CardTypeWithJoker::Q,
        'K' => CardTypeWithJoker::K,
        'A' => CardTypeWithJoker::A,
        _ => panic!("Problem parsing CardType!"),
    }
}

fn get_hand_type<T>(cards:&Vec<T>) -> HandType  where T:Ord, T:Hash, T:Copy{
    if cards.len() != 5 {
        panic!("Hand size is {}, should be 5", {cards.len()});
    }
    let reduced_cards:HashMap<T, u32> = reduce_cards(cards);
    let card_counts:Vec<&u32> = reduced_cards.values().collect();
    match card_counts.len() {
        1 => HandType::FiveOfAKind,
        2 => if card_counts.contains(&&4) {
            HandType::FourOfAKind
        } else {
            HandType::FullHouse
        },
        3 => if card_counts.contains(&&3) {
            HandType::ThreeOfAKind
        } else {
            HandType::TwoPair
        },
        4 => HandType::OnePair,
        5 => HandType::HighCard,
        _ => panic!("Problems parsing hand type!"),
    }
}

fn resolve_jokers(cards:&Vec<CardTypeWithJoker>) -> Option<Vec<CardTypeWithJoker>> {
    let mut reduced_hand:HashMap<CardTypeWithJoker, u32> = reduce_cards(&cards);
    if reduced_hand.contains_key(&CardTypeWithJoker::J) {
        let joker_amnt:u32 = *reduced_hand.get(&CardTypeWithJoker::J).unwrap();
        if joker_amnt == 5 {
            Some(vec![CardTypeWithJoker::A, CardTypeWithJoker::A, CardTypeWithJoker::A, CardTypeWithJoker::A, CardTypeWithJoker::A])
        } else {
            reduced_hand.remove(&CardTypeWithJoker::J);
            let mut most_common_card_type:&CardTypeWithJoker = &CardTypeWithJoker::A;
            let mut most_common_card_amount:u32 = 0;
            for k in reduced_hand.keys() {
                let k_amnt:u32 = *reduced_hand.get(k).unwrap();
                if k_amnt > most_common_card_amount {
                    most_common_card_type = k;
                    most_common_card_amount = k_amnt;
                }
            }
            Some(cards.iter().map(|ct| if ct == &CardTypeWithJoker::J {
                *most_common_card_type
            } else {
                *ct
            }).collect())
        }
    } else {
        None
    }
}

fn reduce_cards<T>(cards:&Vec<T>) -> HashMap<T, u32> where T:Eq, T:Hash, T:Copy {
    let mut accumulator:HashMap<T, u32> = HashMap::new();
    for card in cards {
        if accumulator.contains_key(&card) {
            accumulator.insert(*card, accumulator.get(&card).unwrap() + 1);
        } else {
            accumulator.insert(*card, 1);
        }
    }
    accumulator
}

fn get_score<T>(hands:&Vec<Hand<T>>) -> u32 where T:Ord, T:Hash, T:Copy{
    hands.iter().zip(1..=hands.len() as u32).fold(0, |acc, (h, i)| acc + h.bid * i)
}