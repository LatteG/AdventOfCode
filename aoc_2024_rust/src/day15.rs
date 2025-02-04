use std::{fmt::Debug, io::Empty};

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
enum WideTile {
    Wall,
    BoxLeft,
    BoxRight,
    Robot,
    Empty
}

impl Debug for WideTile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WideTile::Wall     => write!(f, "#"),
            WideTile::BoxLeft  => write!(f, "["),
            WideTile::BoxRight => write!(f, "]"),
            WideTile::Robot    => write!(f, "@"),
            WideTile::Empty    => write!(f, "."),
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
type WideWarehouse = Vec<Vec<WideTile>>;

pub fn task1(input:&str) {
    let (warehouse, moves): (Warehouse, Vec<Direction>) = parse_input(input);
    let robot_pos: Coord = get_robot_pos(&warehouse);
    let (warehouse_after_moves, _): (Warehouse, Coord) = moves.iter().fold((warehouse.clone(), robot_pos), move_robot);
    println!("GPS sum: {}", calc_gps_score(&warehouse_after_moves));
}

pub fn task2(input:&str) {
    let (warehouse, moves): (Warehouse, Vec<Direction>) = parse_input(input);
    let wide_warehouse: WideWarehouse = enwiden_warehouse(warehouse);
    let robot_pos: Coord = get_robot_wide_pos(&wide_warehouse);
    let (warehouse_after_moves, _): (WideWarehouse, Coord) = moves.iter().fold((wide_warehouse.clone(), robot_pos), move_robot_wide);

    println!("Wide GPS sum: {}", calc_wide_gps_score(&warehouse_after_moves));
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

fn calc_wide_gps_score(warehouse:&WideWarehouse) -> i32 {
    let mut gps_cum_sum: i32 = 0;
    for y in 0..warehouse.len() {
        for x in 0..warehouse[0].len() {
            match warehouse[y as usize][x as usize] {
                WideTile::BoxLeft => gps_cum_sum += x as i32 + 100 * y as i32,
                _ => continue
            }
        }
    }
    gps_cum_sum
}

fn print_wide_warehouse(ww:&WideWarehouse) {
    for line in ww {
        println!("{:?}", line);
    }
}

fn move_robot_wide((mut warehouse, robot_pos @ (rx, ry)):(WideWarehouse, Coord), move_direction:&Direction) -> (WideWarehouse, Coord) {
    let (dx, dy): Coord = move_direction.get_delta_coord();
    let next_robot_pos @ (nrx, nry): Coord = (rx + dx, ry + dy);
    match warehouse[nry as usize][nrx as usize] {
        WideTile::Robot => panic!("A second robot has been found."),
        WideTile::Wall  => (warehouse, robot_pos),
        WideTile::Empty => {
            warehouse[nry as usize][nrx as usize] = WideTile::Robot;
            warehouse[ry as usize][rx as usize] = WideTile::Empty;
            (warehouse, next_robot_pos)
        },
        WideTile::BoxLeft | WideTile::BoxRight   => {
            if *move_direction == Direction::Left || *move_direction == Direction::Right {
                let mut box_stop_x: i32 = nrx + dx;
                let mut box_stop_y: i32 = nry + dy;
                while vec![WideTile::BoxLeft, WideTile::BoxRight].contains(&warehouse[box_stop_y as usize][box_stop_x as usize]) {
                    box_stop_x += dx;
                    box_stop_y += dy;
                }
                match warehouse[box_stop_y as usize][box_stop_x as usize] {
                    WideTile::Wall  => {
                        (warehouse, robot_pos)
                    },
                    WideTile::Empty => {
                        if *move_direction == Direction::Left {
                            for x in box_stop_x..rx {
                                warehouse[box_stop_y as usize][x as usize] = warehouse[box_stop_y as usize][(x + 1) as usize].clone();
                            }
                        } else {
                            for x in (nrx..(box_stop_x + 1)).rev() {
                                warehouse[box_stop_y as usize][x as usize] = warehouse[box_stop_y as usize][(x - 1) as usize].clone();
                            }
                        }
                        warehouse[ry as usize][rx as usize] = WideTile::Empty;
                        (warehouse, next_robot_pos)
                    },
                    _ => panic!("Something went wrong when trying to move boxes."),
                }
            } else {
                let mut box_pos_to_update: Vec<Coord> = Vec::new();
                let mut boxes_to_check: Vec<Coord> = Vec::new();
                let mut crashed: bool = false;
                if warehouse[nry as usize][nrx as usize] == WideTile::BoxLeft {
                    boxes_to_check.push((nrx, nry));
                } else {
                    boxes_to_check.push((nrx - 1, nry));
                }
                while boxes_to_check.len() > 0 && !crashed{
                    box_pos_to_update.append(&mut boxes_to_check.clone());
                    boxes_to_check = boxes_to_check.into_iter().flat_map(|(x, y)| {
                        match warehouse[(y + dy) as usize][x as usize] {
                            WideTile::Wall => {
                                crashed = true;
                                vec![]
                            },
                            WideTile::Robot => panic!("Something went wrong when trying to move boxes."),
                            WideTile::BoxLeft => vec![(x, y + dy)],
                            WideTile::BoxRight | WideTile::Empty => {
                                let mut return_vec: Vec<(i32, i32)> = if warehouse[(y + dy) as usize][x as usize] == WideTile::Empty {vec![]} else {vec![(x - 1, y + dy)]};
                                match warehouse[(y + dy) as usize][(x + 1) as usize] {
                                    WideTile::Wall => {
                                        crashed = true;
                                        while !return_vec.is_empty() {
                                            return_vec.remove(0);
                                        }
                                    },
                                    WideTile::Empty => (),
                                    WideTile::BoxLeft => return_vec.push((x + 1, y + dy)),
                                    _ => panic!("Something went wrong when trying to move boxes.")
                                }
                                return_vec
                            },
                        }
                    }).collect();
                }
                if crashed {
                    // Don't move because we hit a wall
                    (warehouse, robot_pos)
                } else {
                    // Move all the connected boxes
                    for (x, y) in box_pos_to_update.iter().rev() {
                        warehouse[(*y + dy) as usize][*x as usize] = WideTile::BoxLeft;
                        warehouse[(*y + dy) as usize][(*x + 1) as usize] = WideTile::BoxRight;
                        warehouse[*y as usize][*x as usize] = WideTile::Empty;
                        warehouse[*y as usize][(*x + 1) as usize] = WideTile::Empty;
                    }
                    // Move the robot
                    warehouse[nry as usize][nrx as usize] = WideTile::Robot;
                    warehouse[ry as usize][rx as usize] = WideTile::Empty;
                    (warehouse, next_robot_pos)
                }
            }
        }
    }
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

fn get_robot_wide_pos(warehouse:&WideWarehouse) -> Coord {
    let y: usize = warehouse.iter().position(|line| line.contains(&WideTile::Robot)).unwrap();
    let x: usize = warehouse[y].iter().position(|tile| *tile == WideTile::Robot).unwrap();
    (x as i32, y as i32)
}

fn enwiden_warehouse(warehouse:Warehouse) -> WideWarehouse {
    warehouse.iter().map(|line| line.iter().flat_map(|tile| match tile {
        Tile::Wall  => vec![WideTile::Wall, WideTile::Wall],
        Tile::Box   => vec![WideTile::BoxLeft, WideTile::BoxRight],
        Tile::Robot => vec![WideTile::Robot, WideTile::Empty],
        Tile::Empty => vec![WideTile::Empty, WideTile::Empty],
    }).collect()).collect()
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
