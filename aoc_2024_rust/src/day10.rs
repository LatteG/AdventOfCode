use std::collections::HashSet;

enum Direction {
    North,
    East,
    South,
    West
}

impl Direction {
    fn get_delta_coord(&self) -> Coord {
        match self {
            Direction::North => ( 0, -1),
            Direction::East  => ( 1,  0),
            Direction::South => ( 0,  1),
            Direction::West  => (-1,  0),
        }
    }
}

type Coord = (i32, i32);
type TrailMap = Vec<Vec<u8>>;

pub fn task1(input:&str) {
    let trail_map: TrailMap = parse_input(input);
    let trail_heads: Vec<Coord> = find_trail_heads(&trail_map);
    let trail_head_score: u32 = trail_heads.iter().fold(0,|acc, head_coord| acc + get_trail_head_score(head_coord, &trail_map));
    println!("Trail head score sum:\n{}", trail_head_score);
}

pub fn task2(input:&str) {
    let trail_map: TrailMap = parse_input(input);
    let trail_heads: Vec<Coord> = find_trail_heads(&trail_map);
    let trail_head_rating: u32 = trail_heads.iter().fold(0,|acc, head_coord| acc + get_trail_head_rating(head_coord, &trail_map));
    println!("Trail head rating sum:\n{}", trail_head_rating);
}

fn get_trail_head_rating(pos @ (x, y):&Coord, trail_map:&TrailMap) -> u32 {
    let height: u8 = trail_map[*y as usize][*x as usize];
    if height == 9 {
        1
    } else {
        let mut neighbours: Vec<Coord> = Vec::new();
        for (dx, dy) in [Direction::North, Direction::East, Direction::South, Direction::West].iter().map(|dir| dir.get_delta_coord()) {
            let nx: i32 = x + dx;
            let ny: i32 = y + dy;
            if 0 <= nx && nx < trail_map[0].len() as i32 && 0 <= ny && ny < trail_map.len() as i32 && trail_map[ny as usize][nx as usize] == height + 1 {
                neighbours.push((nx, ny));
            }
        }
        if neighbours.len() == 0 {
            0
        } else {
            neighbours.into_iter().map(|n_coord| get_trail_head_rating(&n_coord, trail_map)).reduce(|acc, val| acc + val).unwrap()
        }
    }
}

fn get_trail_head_score(head_coord:&Coord, trail_map:&TrailMap) -> u32 {
    let peaks: HashSet<Coord> = get_peaks(head_coord, trail_map);
    peaks.len() as u32
}

fn get_peaks(pos @ (x, y):&Coord, trail_map:&TrailMap) -> HashSet<Coord> {
    let height: u8 = trail_map[*y as usize][*x as usize];
    if height == 9 {
        HashSet::from([*pos])
    } else {
        let mut neighbours: Vec<Coord> = Vec::new();
        for (dx, dy) in [Direction::North, Direction::East, Direction::South, Direction::West].iter().map(|dir| dir.get_delta_coord()) {
            let nx: i32 = x + dx;
            let ny: i32 = y + dy;
            if 0 <= nx && nx < trail_map[0].len() as i32 && 0 <= ny && ny < trail_map.len() as i32 && trail_map[ny as usize][nx as usize] == height + 1 {
                neighbours.push((nx, ny));
            }
        }
        if neighbours.len() == 0 {
            HashSet::new()
        } else {
            neighbours.into_iter().map(|n_coord| get_peaks(&n_coord, trail_map)).reduce(|acc, set| acc.union(&set).copied().collect()).unwrap()
        }
    }
}

fn find_trail_heads(trail_map:&TrailMap) -> Vec<Coord> {
    let mut trail_heads: Vec<Coord> = Vec::new();
    for y in 0..trail_map.len() {
        for x in 0..trail_map[0].len() {
            if trail_map[y][x] == 0 {
                trail_heads.push((x as i32, y as i32));
            }
        }
    }
    trail_heads
}

fn parse_input(input:&str) -> TrailMap {
    input.split("\n").map(|line_str| line_str.chars().map(|tile| tile.to_string().parse::<u8>().unwrap()).collect()).collect()
}
