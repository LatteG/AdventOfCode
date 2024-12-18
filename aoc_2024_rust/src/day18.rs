use std::fmt::Debug;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Tile {
    Empty,
    Corrupt,
    Visited
}

impl ToString for Tile {
    fn to_string(&self) -> String {
        match self {
            Self::Empty   => ".".to_string(),
            Self::Corrupt => "#".to_string(),
            Self::Visited => "O".to_string(),
        }
    }
}

impl Debug for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd)]
enum Direction {
    North,
    East,
    South,
    West
}

impl Direction {
    fn get_delta_coord(self) -> (i32, i32) {
        match self {
            Direction::North => ( 0, -1),
            Direction::East  => ( 1,  0),
            Direction::South => ( 0,  1),
            Direction::West  => (-1,  0),
        }
    }

    fn get_clockwise(self) -> Direction {
        match self {
            Direction::North => Direction::East,
            Direction::East  => Direction::South,
            Direction::South => Direction::West,
            Direction::West  => Direction::North,
        }
    }

    fn get_counterclockwise(self) -> Direction {
        match self {
            Direction::North => Direction::West,
            Direction::East  => Direction::North,
            Direction::South => Direction::East,
            Direction::West  => Direction::South,
        }
    }
}

type Coord = (usize, usize);
type RAM = Vec<Vec<Tile>>;

pub fn task1(input:&str) {
    let bytes: Vec<Coord> = parse_input(input);
    let (ram_size, push_length): (Coord, usize) = get_ram_and_push_size(&bytes);
    let ram: RAM = build_ram(&bytes, push_length, ram_size);
    let path_len: usize = exit_ram(ram.clone(), ram_size).unwrap();
    println!("It takes minimum {} steps to exit the RAM", path_len);
}

pub fn task2(input:&str) {
    let bytes: Vec<Coord> = parse_input(input);
    let (ram_size, push_length): (Coord, usize) = get_ram_and_push_size(&bytes);
    let ram: RAM = build_ram(&bytes, push_length, ram_size);
    let cutoff_coord = find_exit_cutoff(ram.clone(), &bytes, ram_size, push_length).unwrap();
    println!("The RAM is cutoff after corrupting the byte at: {:?}", cutoff_coord);
}

fn print_ram(ram:&RAM) {
    let tile_strs: Vec<Vec<String>> = ram.iter().map(|line| line.iter().map(|tile| tile.to_string()).collect()).collect();
    let line_strs: Vec<String> = tile_strs.iter().map(|line| line.join("")).collect();
    let ram_str: String = line_strs.join("\n");
    println!("{}", ram_str);
}

fn find_exit_cutoff(mut ram:RAM, bytes:&Vec<Coord>, ram_size:Coord, push_length:usize) -> Option<Coord> {
    for i in push_length..bytes.len() {
        let byte_coord @ (bx, by): Coord = bytes[i];
        ram[by][bx] = Tile::Corrupt;
        if exit_ram(ram.clone(), ram_size) == None {
            return Some(byte_coord);
        }
    }
    None
}

fn exit_ram(mut ram:RAM, (ram_width, ram_height):Coord) -> Option<usize> {
    let mut searchers: Vec<(Coord, Vec<Coord>, Direction)> = vec![((0,0), vec![], Direction::East)];
    let goal: Coord = (ram_width - 1, ram_height - 1);

    while searchers.len() > 0 {
        match searchers.iter().position(|(c, _, _)| c == &goal) {
            Some(finalist_index) =>
                return Some(searchers[finalist_index].1.len()),
            None => {
                searchers = searchers.iter().flat_map(|((x, y), path, facing)| {
                    let mut acc: Vec<(Coord, Vec<Coord>, Direction)> = Vec::new();
                    for dir in [facing.get_counterclockwise(), *facing, facing.get_clockwise()] {
                        let (dx, dy): (i32, i32) = dir.get_delta_coord();
                        if 0 <= *x as i32 + dx && 0 <= *y as i32 + dy {
                            let new_coord @ (new_x, new_y): Coord = ((*x as i32 + dx) as usize, (*y as i32 + dy) as usize);
                            if new_x < ram_width && new_y < ram_height {
                                if ram[new_y][new_x] == Tile::Empty {
                                    ram[new_y][new_x] = Tile::Visited;
                                    acc.push((new_coord, [path.clone(), vec![new_coord]].concat(), dir));
                                }
                            }
                        }
                    }
                    acc
                }).collect();
            },
        }
    }
    None
}

fn build_ram(bytes:&Vec<Coord>, bytes_to_push:usize, (ram_width, ram_height):Coord) -> RAM {
    let mut ram: RAM = vec![vec![Tile::Empty; ram_width]; ram_height];
    for i in 0..bytes_to_push {
        let (x, y): Coord = bytes[i];
        ram[y][x] = Tile::Corrupt;
    }
    ram
}

fn get_ram_and_push_size(bytes:&Vec<Coord>) -> (Coord, usize) {
    if bytes.len() < 100 {
        ((7, 7), 12)
    } else {
        ((71, 71), 1024)
    }
}

fn parse_input(input:&str) -> Vec<Coord> {
    input.lines().map(|coord_str|{
        let (x_str, y_str): (&str, &str) = coord_str.split_once(",").unwrap();
        (x_str.parse::<usize>().unwrap(), y_str.parse::<usize>().unwrap())
    }).collect()
}
