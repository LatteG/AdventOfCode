struct Hand {
    red: u32,
    green: u32,
    blue: u32,
}

struct Game {
    id: u32,
    hands: Vec<Hand>,
}

impl Game {
    fn max_red(&self) -> u32 {
        self.hands.iter().map(|h| h.red).max().unwrap()
    }
    fn max_green(&self) -> u32 {
        self.hands.iter().map(|h| h.green).max().unwrap()
    }
    fn max_blue(&self) -> u32 {
        self.hands.iter().map(|h| h.blue).max().unwrap()
    }
    fn min_rocks_points(&self, red:u32, green:u32, blue:u32) -> u32 {
        let check:bool = red   >= self.max_red() && 
                         green >= self.max_green() && 
                         blue  >= self.max_blue();
        if check {
            self.id
        } else {
            0
        }
    }
    fn power(&self) -> u32 {
        self.max_red() * self.max_blue() * self.max_green()
    }
}

pub fn task1(input:&str) {
    let games:Vec<Game> = parse_games(input);
    let score:u32 = games.iter().map(|game| game.min_rocks_points(12, 13, 14)).sum();
    println!("{}", score);
}

pub fn task2(input:&str) {
    let games:Vec<Game> = parse_games(input);
    let score:u32 = games.iter().map(|game| game.power()).sum();
    println!("{}", score);
}

fn parse_games(input:&str) -> Vec<Game> {
    input.split("\n").map(parse_game).collect()
}

fn parse_game(game_str:&str) -> Game {
    let (game_head, hands_string):(&str, &str) = game_str.split_once(": ").unwrap();
    let game_id:u32 = match game_head.split_once(" ").unwrap().1.parse() {
        Ok(id) => id,
        Err(..) => panic!("No game ID for {}", game_str),
    };
    let hands:Vec<Hand> = parse_hands(hands_string);
    Game {
        id: game_id,
        hands: hands,
    }
}

fn parse_hands(hands_string:&str) -> Vec<Hand> {
    hands_string.split("; ").map(parse_hand).collect()
}

fn parse_hand(hand_string:&str) -> Hand {
    let colours:Vec<&str> = hand_string.split(", ").collect();
    let red:u32 = get_colour("red", &colours);
    let green:u32 = get_colour("green", &colours);
    let blue:u32 = get_colour("blue", &colours);

    Hand {
        red: red,
        blue: blue,
        green: green,
    }
}

fn get_colour(colour:&str, colours:&Vec<&str>) -> u32 {
    colours.iter().map(|c_str| if c_str.contains(colour) {
        match c_str.split_once(" ").unwrap().0.parse() {
            Ok(num) => num,
            Err(..) => panic!("No rocks for colour!"),
        }
    } else {0}).sum()
}