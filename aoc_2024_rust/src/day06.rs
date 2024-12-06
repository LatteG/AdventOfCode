use std::{fmt::Debug, str::FromStr};

#[derive(Copy, Clone, PartialEq, Eq)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl Direction {
    fn get_coordinate_offset(&self) -> (i32, i32) {
        match self {
            Direction::Up => (0, -1),
            Direction::Right => (1, 0),
            Direction::Down => (0, 1),
            Direction::Left => (-1, 0),
        }
    }

    fn rotate(&self) -> Direction {
        match self {
            Direction::Up => Direction::Right,
            Direction::Right => Direction::Down,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
        }
    }
}

impl ToString for Direction {
    fn to_string(&self) -> String {
        match self {
            Direction::Up => String::from_str("^").unwrap(),
            Direction::Right => String::from_str(">").unwrap(),
            Direction::Down => String::from_str("v").unwrap(),
            Direction::Left => String::from_str("<").unwrap(),
        }
    }
}

impl Debug for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Tile {
    Empty,
    Visited,
    Guard(Direction),
    Obstacle,
}

impl ToString for Tile {
    fn to_string(&self) -> String {
        match self {
            Tile::Empty => String::from_str(".").unwrap(),
            Tile::Visited => String::from_str("X").unwrap(),
            Tile::Guard(direction) => direction.to_string(),
            Tile::Obstacle => String::from_str("#").unwrap(),
        }
    }
}

impl Debug for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Tile {
    fn is_guard(&self) -> bool {
        match self {
            Tile::Guard(_) => true,
            _ => false,
        }
    }
}

#[derive(Copy, Clone)]
struct Guard {
    direction: Direction,
    position: (usize, usize),
}

impl Guard {
    fn step(&self, tile_map: &TileMap) -> (Option<Guard>, TileMap) {
        let (delta_x, delta_y): (i32, i32) = self.direction.get_coordinate_offset();

        let next_x: i32 = self.position.0 as i32 + delta_x;
        let next_y: i32 = self.position.1 as i32 + delta_y;

        let mut return_map: TileMap = tile_map.clone();

        return_map[self.position.1][self.position.0] = Tile::Visited;

        if next_x < 0 || next_x >= return_map[0].len() as i32 || next_y < 0 || next_y >= return_map.len() as i32 {
            (None, return_map)
        } else if return_map[next_y as usize][next_x as usize] == Tile::Obstacle {
            let rotated_guard: Guard = Guard { direction: self.direction.rotate(), position: self.position };
            rotated_guard.step(&return_map)
        } else {
            let moved_guard: Guard = Guard { direction: self.direction, position: (next_x as usize, next_y as usize) };
            return_map[next_y as usize][next_x as usize] = Tile::Guard(self.direction);
            (Some(moved_guard), return_map)
        }
    }
}

impl Debug for Guard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Guard").field("direction", &self.direction).field("position", &self.position).finish()
    }
}

type TileMap = Vec<Vec<Tile>>;

pub fn task1(input:&str) {
    let (guard, tile_map): (Guard, TileMap) = parse_input(input);
    // print_tile_map(&tile_map);
    // println!("{:#?}", guard);
    let final_map: TileMap = move_guard(guard, tile_map);
    // print_tile_map(&final_map);
    let visited_tile_count: i32 = count_visited_tiles(&final_map);
    println!("The guard visited {} tiles", visited_tile_count);
}

pub fn task2(input:&str) {
    println!("TODO: Task 2")
}

fn count_visited_tiles(tile_map: &TileMap) -> i32{
    tile_map.iter().fold(0, |acc, line| acc + line.iter().fold(0, |acc, tile| if *tile == Tile::Visited {acc + 1} else {acc}))
}

fn move_guard(mut guard: Guard, mut tile_map: TileMap) -> TileMap {
    loop {
        match guard.step(&tile_map) {
            (None, return_map) => return return_map,
            (Some(moved_guard), updated_map) => (guard, tile_map) = (moved_guard, updated_map),
        }
    }
}

fn print_tile_map(tile_map: &TileMap) {
    let line_strs: Vec<String> = tile_map.iter().map(|tile_line| tile_line.iter().map(|tile| tile.to_string()).collect()).collect();
    let tile_map_str: String = line_strs.join("\n");
    println!("{}", tile_map_str);
}

fn parse_input(input:&str) -> (Guard, TileMap) {
    let lines: Vec<&str> = input.split("\n").collect();
    let tile_map: TileMap = lines.into_iter().map(parse_line).collect();

    let mut guard_coord: (usize, usize) = (0, 0);

    for y in 0..tile_map.len() {
        match tile_map[y].iter().position(|tile| tile.is_guard()) {
            Some(x) => {
                guard_coord = (x, y);
                break;
            },
            None => (),
        }
    }
    
    match &tile_map[guard_coord.1][guard_coord.0] {
        &Tile::Guard(dir) => (Guard{direction: dir, position: guard_coord}, tile_map),
        _ => panic!("Problem parsing input!"),
    }
}

fn parse_line(line:&str) -> Vec<Tile> {
    line.chars().map(parse_tile).collect()
}

fn parse_tile(tile_char:char) -> Tile {
    match tile_char {
        '.' => Tile::Empty,
        'X' => Tile::Visited,
        '#' => Tile::Obstacle,
        '^' => Tile::Guard(Direction::Up),
        '>' => Tile::Guard(Direction::Right),
        'v' => Tile::Guard(Direction::Down),
        '<' => Tile::Guard(Direction::Left),
        _ => panic!("Unknown tile found: {}", tile_char),
    }
}
