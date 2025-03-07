use std::{collections::HashMap, fmt::Debug, usize};

use memoize::memoize;

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
enum KeyStroke {
    Zero,
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Activate,
    Left,
    Up,
    Down,
    Right
}

impl ToString for KeyStroke {
    fn to_string(&self) -> String {
        match self {
            Self::Zero     => "0".to_string(),
            Self::One      => "1".to_string(),
            Self::Two      => "2".to_string(),
            Self::Three    => "3".to_string(),
            Self::Four     => "4".to_string(),
            Self::Five     => "5".to_string(),
            Self::Six      => "6".to_string(),
            Self::Seven    => "7".to_string(),
            Self::Eight    => "8".to_string(),
            Self::Nine     => "9".to_string(),
            Self::Activate => "A".to_string(),
            Self::Left     => "<".to_string(),
            Self::Up       => "^".to_string(),
            Self::Right    => ">".to_string(),
            Self::Down     => "v".to_string()
        }
    }
}

impl Debug for KeyStroke {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl KeyStroke {
    fn from_char(c:char) -> Option<KeyStroke> {
        KeyStroke::from_str(c.to_string().as_str())
    }
    fn from_string(str:String) -> Option<KeyStroke> {
        KeyStroke::from_str(str.as_str())
    }
    fn from_str(str:&str) -> Option<KeyStroke> {
        match str {
            "0" => Some(Self::Zero),
            "1" => Some(Self::One),
            "2" => Some(Self::Two),
            "3" => Some(Self::Three),
            "4" => Some(Self::Four),
            "5" => Some(Self::Five),
            "6" => Some(Self::Six),
            "7" => Some(Self::Seven),
            "8" => Some(Self::Eight),
            "9" => Some(Self::Nine),
            "A" => Some(Self::Activate),
            "<" => Some(Self::Left),
            "^" => Some(Self::Up),
            ">" => Some(Self::Right),
            "v" => Some(Self::Down),
            _   => None
        }
    }
}

type Coord = (usize, usize);
type Cache = HashMap<Vec<KeyStroke>, Vec<KeyStroke>>;

#[derive(Debug, Clone)]
struct KeyPad {
    keys: HashMap<KeyStroke, Coord>,
    empty: Vec<Coord>
}

impl KeyPad {
    fn get_strokes(self, curr_key:KeyStroke, next_key:KeyStroke) -> Vec<KeyStroke> {
        let (cx, cy): Coord = self.keys[&curr_key];
        let (nx, ny): Coord = self.keys[&next_key];
        let mut strokes: Vec<KeyStroke> = Vec::new();
        if cx > nx {
            strokes.append(&mut vec![KeyStroke::Left; cx - nx]);
        }
        if cy < ny {
            strokes.append(&mut vec![KeyStroke::Down; ny - cy]);
        }
        if cy > ny {
            strokes.append(&mut vec![KeyStroke::Up; cy - ny]);
        }
        if cx < nx {
            strokes.append(&mut vec![KeyStroke::Right; nx - cx]);
        }
        if self.passes_empty(strokes.clone(), curr_key) {
            strokes.reverse();
        }
        strokes.push(KeyStroke::Activate);
        strokes
    }

    fn passes_empty(self, strokes:Vec<KeyStroke>, curr_key:KeyStroke) -> bool {
        let (mut x, mut y): Coord = *self.keys.get(&curr_key).unwrap();
        for stroke in strokes {
            match stroke {
                KeyStroke::Left  => x -= 1,
                KeyStroke::Right => x += 1,
                KeyStroke::Up    => y -= 1,
                KeyStroke::Down  => y += 1,
                _ => panic!("Unexpected stroke!")
            }
            if self.empty.contains(&(x, y)) {
                return true;
            }
        }
        false
    }
}

pub fn task1(input:&str) {
    let keypads: Vec<KeyPad> = get_keypads(3);
    let sequences: Vec<Vec<KeyStroke>> = parse_input(input);
    let human_sequences: Vec<Vec<KeyStroke>> = sequences.iter().map(|seq| expand_sequence(&keypads, seq.clone())).collect();
    let mut complexity: u32 = 0;
    for i in 0..sequences.len() {
        // println!("{:?} ({}): {:?}", sequences[i], human_sequences[i].len(), human_sequences[i]);
        complexity += calc_complexity(sequences[i].clone(), human_sequences[i].clone());
    }
    println!("Complexity: {}", complexity);
}

pub fn task2(input:&str) {
    let keypads: Vec<KeyPad> = get_keypads(26);
    let sequences: Vec<Vec<KeyStroke>> = parse_input(input);
    let mut cache: Cache = HashMap::new();
    let human_sequences: Vec<Vec<KeyStroke>> = sequences.iter().map(|seq| cached_expand_sequence(&keypads, seq.clone(), &mut cache)).collect();
    let mut complexity: u32 = 0;
    for i in 0..sequences.len() {
        // println!("{:?} ({}): {:?}", sequences[i], human_sequences[i].len(), human_sequences[i]);
        complexity += calc_complexity(sequences[i].clone(), human_sequences[i].clone());
    }
    println!("Complexity: {}", complexity);
}

fn calc_complexity(num_seq:Vec<KeyStroke>, human_seq:Vec<KeyStroke>) -> u32 {
    let num_str: String = format!("{:?}{:?}{:?}", num_seq[0], num_seq[1], num_seq[2]);
    let num: u32 = num_str.parse::<u32>().unwrap();
    num * human_seq.len() as u32
}

fn expand_sequence(keypads:&Vec<KeyPad>, mut seq: Vec<KeyStroke>) -> Vec<KeyStroke> {
    // println!("{:?}", seq);
    for pad in keypads {
        let mut curr_key: KeyStroke = KeyStroke::Activate;
        let mut next_seq: Vec<KeyStroke> = Vec::new();
        for next_key in seq.iter() {
            let mut seq: Vec<KeyStroke> = pad.clone().get_strokes(curr_key, *next_key);
            next_seq.append(&mut seq);
            curr_key = *next_key;
        }
        seq = next_seq
        // println!("{:?}", seq);
    }
    seq
}

fn cached_expand_sequence(keypads:&Vec<KeyPad>, mut seq: Vec<KeyStroke>, cache:&mut Cache) -> Vec<KeyStroke> {
    // println!("{:?}", seq);
    for pad in keypads {
        let mut expanded_seqs: Vec<Vec<KeyStroke>> = Vec::new();
        for key_seq in seq.split(|&ks| ks == KeyStroke::Activate) {
            let key_vec: Vec<KeyStroke> = key_seq.to_vec();
            match cache.get(&key_vec) {
                Some(next_vec) => expanded_seqs.push(next_vec.clone()),
                None => {
                    
                },
            }
        }
    }
    seq
}

fn get_keypads(directional_pads:usize) -> Vec<KeyPad> {
    let mut keypads: Vec<KeyPad> = Vec::new();
    // The numeric keypad
    keypads.push(KeyPad{
        keys: HashMap::from([
            (KeyStroke::Seven, (0, 0)), (KeyStroke::Eight, (1, 0)), (KeyStroke::Nine,     (2, 0)),
            (KeyStroke::Four,  (0, 1)), (KeyStroke::Five,  (1, 1)), (KeyStroke::Six,      (2, 1)),
            (KeyStroke::One,   (0, 2)), (KeyStroke::Two,   (1, 2)), (KeyStroke::Three,    (2, 2)),
                                        (KeyStroke::Zero,  (1, 3)), (KeyStroke::Activate, (2, 3)),
        ]),
        empty: vec![(0, 3)]
    });
    // The directional keypads
    for _ in 1..directional_pads {
        keypads.push(KeyPad{
            keys:  HashMap::from([
                                           (KeyStroke::Up,   (1, 0)), (KeyStroke::Activate, (2, 0)),
                (KeyStroke::Left, (0, 1)), (KeyStroke::Down, (1, 1)), (KeyStroke::Right,    (2, 1))
            ]),
            empty: vec![(0, 0)]
        });
    }
    keypads
}

fn parse_input(input:&str) -> Vec<Vec<KeyStroke>> {
    input.lines().map(|line| line.chars().map(|char| KeyStroke::from_char(char).unwrap()).collect()).collect()
}
