use std::fmt::Debug;

#[derive(Clone, Eq, PartialEq)]
enum Tile {
    Wall,
    Box,
    Robot,
    Empty
}

impl Debug for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tile::Wall  => write!(f, "#"),
            Tile::Box   => write!(f, "O"),
            Tile::Robot => write!(f, "@"),
            Tile::Empty => write!(f, "."),
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
enum Direction {
    Up,
    Right,
    Down,
    Left
}

impl Direction {
    fn get_delta_coord(&self) -> Coord {
        match self {
            Direction::Up    => ( 0, -1),
            Direction::Right => ( 1,  0),
            Direction::Down  => ( 0,  1),
            Direction::Left  => (-1,  0),
        }
    }
}

impl Debug for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Direction::Up    => write!(f, "^"),
            Direction::Right => write!(f, ">"),
            Direction::Down  => write!(f, "v"),
            Direction::Left  => write!(f, "<"),
        }
    }
}

type Coord = (i32, i32);
type Warehouse = Vec<Vec<Tile>>;

pub fn task1(input:&str) {
    let (warehouse, moves): (Warehouse, Vec<Direction>) = parse_input(input);
    let robot_pos: Coord = get_robot_pos(&warehouse);
    let (warehouse_after_moves, _): (Warehouse, Coord) = moves.iter().fold((warehouse.clone(), robot_pos), move_robot);
    println!("GPS sum: {}", calc_gps_score(&warehouse_after_moves));
}

pub fn task2(input:&str) {
    println!("TODO: Task 2")
}

fn calc_gps_score(warehouse:&Warehouse) -> i32 {
    let mut gps_cum_sum: i32 = 0;
    for y in 0..warehouse.len() {
        for x in 0..warehouse[0].len() {
            match warehouse[y as usize][x as usize] {
                Tile::Box => gps_cum_sum += x as i32 + 100 * y as i32,
                _ => continue
            }
        }
    }
    gps_cum_sum
}

fn move_robot((mut warehouse, robot_pos @ (rx, ry)):(Warehouse, Coord), move_direction:&Direction) -> (Warehouse, Coord) {
    let delta_pos @ (dx, dy): Coord = move_direction.get_delta_coord();
    let next_robot_pos @ (nrx, nry): Coord = (rx + dx, ry + dy);
    match warehouse[nry as usize][nrx as usize] {
        Tile::Robot => panic!("A second robot has been found."),
        Tile::Wall  => (warehouse, robot_pos),
        Tile::Empty => {
            warehouse[nry as usize][nrx as usize] = Tile::Robot;
            warehouse[ry as usize][rx as usize] = Tile::Empty;
            (warehouse, next_robot_pos)
        },
        Tile::Box   => {
            let mut box_stop_x: i32 = nrx + dx;
            let mut box_stop_y: i32 = nry + dy;
            while warehouse[box_stop_y as usize][box_stop_x as usize] == Tile::Box {
                box_stop_x += dx;
                box_stop_y += dy;
            }
            match warehouse[box_stop_y as usize][box_stop_x as usize] {
                Tile::Wall  => (warehouse, robot_pos),
                Tile::Empty => {
                    warehouse[box_stop_y as usize][box_stop_x as usize] = Tile::Box;
                    warehouse[nry as usize][nrx as usize] = Tile::Robot;
                    warehouse[ry as usize][rx as usize] = Tile::Empty;
                    (warehouse, next_robot_pos)
                },
                _ => panic!("Something went wrong when trying to move boxes."),
            }
        }
    }
}

fn get_robot_pos(warehouse:&Warehouse) -> Coord {
    let y: usize = warehouse.iter().position(|line| line.contains(&Tile::Robot)).unwrap();
    let x: usize = warehouse[y].iter().position(|tile| *tile == Tile::Robot).unwrap();
    (x as i32, y as i32)
}

fn parse_input(input:&str) -> (Warehouse, Vec<Direction>) {
    let (warehouse_str, moves_str): (&str, &str) = input.split_once("\n\n").unwrap();
    let warehouse: Warehouse = parse_warehouse(warehouse_str);
    let moves: Vec<Direction> = parse_moves(moves_str.replace("\n", ""));
    (warehouse, moves)
}

fn parse_warehouse(warehouse_str:&str) -> Warehouse {
    let lines: Vec<&str> = warehouse_str.lines().collect();
    lines.iter().map(|line| line.chars().map(|tile_char| match tile_char {
        '#' => Tile::Wall,
        'O' => Tile::Box,
        '@' => Tile::Robot,
        '.' => Tile::Empty,
         _  => panic!("Unknown tile found: {}", tile_char)
    }).collect()).collect()
}

fn parse_moves(moves_str:String) -> Vec<Direction> {
    moves_str.chars().map(|dir_char| match dir_char {
        '^' => Direction::Up,
        '>' => Direction::Right,
        'v' => Direction::Down,
        '<' => Direction::Left,
         _  => panic!("Unknown direction found: {}", dir_char)
    }).collect()
}
