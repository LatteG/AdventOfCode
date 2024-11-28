use std::fmt::Debug;

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Clone, Copy)]
enum Tile {
    Vert,
    Hor,
    NE,
    NW,
    SW,
    SE,
    Empty,
    Start,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Clone, Copy)]
enum Direction {
    North,
    East,
    South,
    West,
}

impl Debug for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Vert  => write!(f, "|"),
            Self::Hor   => write!(f, "-"),
            Self::NE    => write!(f, "L"),
            Self::NW    => write!(f, "J"),
            Self::SW    => write!(f, "7"),
            Self::SE    => write!(f, "F"),
            Self::Empty => write!(f, "."),
            Self::Start => write!(f, "S"),
        }
    }
}

impl ToString for Tile {
    fn to_string(&self) -> String {
        match self {
            Self::Vert  => "|",
            Self::Hor   => "-",
            Self::NE    => "L",
            Self::NW    => "J",
            Self::SW    => "7",
            Self::SE    => "F",
            Self::Empty => " ",
            Self::Start => "S",
        }.to_string()
    }
}

impl ToString for Direction {
    fn to_string(&self) -> String {
        match self {
            Direction::North => "North",
            Direction::East  => "East",
            Direction::South => "South",
            Direction::West  => "West",
        }.to_string()
    }
}

type TileMap = Vec<Vec<Tile>>;

pub fn task1(input:&str) {
    let tile_map:TileMap = parse_input(input);
    let (dist, loop_map):(u32, TileMap) = find_dist_to_other_side_of_loop(&tile_map);
    print_tile_map(loop_map);
    println!("Distance to the middle of the loop: {}", dist);
}

pub fn task2(input:&str) {
    let tile_map:TileMap = parse_input(input);
    let (_dist, loop_map):(u32, TileMap) = find_dist_to_other_side_of_loop(&tile_map);
    let enclosed_tile_amount:u32 = get_enclosed_tile_amount(&replace_start_tile(&loop_map));
    println!("Amount of tiles enclosed in loop: {}", enclosed_tile_amount);
}

fn get_enclosed_tile_amount(loop_map:&TileMap) -> u32 {
    // * For each empty tile:
    //   * Get all non-empty tiles between edge and that tile
    //   * Reduce the tiles such that all parallel pipes are removed
    //   * Reduce the tiles such that all turns are either cancelled out or joined into a straight perpendicular pipe
    //   * Count all remaining perpendicular pipes
    //     * If even: Outside
    //     * If odd: Inside
    let mut enclosed_tiles:u32 = 0;
    for y in 0..loop_map.len() {
        let mut acc:Vec<Tile> = Vec::new();
        for x in 0..loop_map[0].len() {
            match loop_map[y][x] {
                Tile::Vert  => acc.push(Tile::Vert),
                Tile::Hor   => (),
                Tile::NE    => acc.push(Tile::NE),
                Tile::SE    => acc.push(Tile::SE),
                Tile::NW    => match acc.pop().unwrap() {
                    Tile::NE => (),
                    Tile::SE => acc.push(Tile::Vert),
                    _ => panic!("Unexpected item in acc!")
                },
                Tile::SW    => match acc.pop().unwrap() {
                    Tile::SE => (),
                    Tile::NE => acc.push(Tile::Vert),
                    _ => panic!("Unexpected item in acc!")
                },
                Tile::Empty => if acc.len() % 2 == 1 {
                    enclosed_tiles += 1;
                },
                Tile::Start => panic!("Start tile not properly removed!"),
            }
        }
    }

    enclosed_tiles
}

fn replace_start_tile(loop_map:&TileMap) -> TileMap {
    let (x,y):(usize, usize) = get_start_tile_coord(loop_map);
    let mut start_directions:Vec<Direction> = Vec::new();
    for (dir, maybe) in get_directions(Tile::Start).iter().map(|dir| (dir, get_tile_at_direction(loop_map, (x, y), dir.clone()))){
        match maybe {
            None => (),
            Some(..) => start_directions.push(dir.clone()),
        }
    };
    start_directions.sort();
    let mut start_tile_replacement:Tile = Tile::Empty;
    for tile in vec![Tile::Hor, Tile::Vert, Tile::NE, Tile::NW, Tile::SE, Tile::SW] {
        if start_directions == get_directions(tile) {
            start_tile_replacement = tile;
        }
    }
    let mut return_map:TileMap = loop_map.clone();
    return_map[y][x] = start_tile_replacement;
    return_map.clone()
}

fn find_dist_to_other_side_of_loop(tile_map:&TileMap) -> (u32, TileMap) {
    let mut loop_map:TileMap = tile_map.iter().map(|l| l.iter().map(|_| Tile::Empty).collect()).collect();

    let (mut x, mut y):(usize, usize) = get_start_tile_coord(tile_map);
    let mut curr_tile:Tile = Tile::Start;
    let mut next_dir:Direction = find_first_direction(tile_map, (x, y));

    let mut loop_size:u32 = 0;
    let mut still_looking:bool = true;

    while still_looking {
        loop_map[y][x] = curr_tile;
        //println!("Step {:5}: {} ({}, {}) {}", loop_size, curr_tile.to_string(), x, y, next_dir.to_string());
        (curr_tile, (x, y)) = get_tile_at_direction(tile_map, (x, y), next_dir).unwrap();
        next_dir = *get_directions(curr_tile).iter().filter(|t| *t != &get_opposite_dir(next_dir)).collect::<Vec<&Direction>>()[0];
        loop_size += 1;
        still_looking = curr_tile != Tile::Start;
    }

    (loop_size / 2, loop_map)
}

fn get_start_tile_coord(tile_map:&TileMap) -> (usize, usize) {
    let y:usize = tile_map.iter().position(|line| line.contains(&Tile::Start)).unwrap();
    let x:usize = tile_map[y].iter().position(|tile| tile == &Tile::Start).unwrap();
    return (x, y);
}

fn get_opposite_dir(dir:Direction) -> Direction {
    match dir {
        Direction::North => Direction::South,
        Direction::East  => Direction::West,
        Direction::South => Direction::North,
        Direction::West  => Direction::East,
    }
}

fn find_first_direction(tile_map:&TileMap, coord:(usize, usize)) -> Direction {
    for direction in get_directions(Tile::Start) {
        match get_tile_at_direction(tile_map, coord, direction) {
            None => (),
            Some((Tile::Empty, _)) => (),
            Some(_) => return direction.clone(),
        }
    }
    panic!("No tile connected to starting tile!");
}

fn get_tile_at_direction(tile_map:&TileMap, coord:(usize, usize), direction:Direction) -> Option<(Tile, (usize, usize))> {
    let (x, y):(usize, usize) = get_new_coord(coord, direction);
    if x >= tile_map[0].len() || y >= tile_map.len() {
        None
    } else {
        if get_directions(tile_map[y][x]).contains(&get_opposite_dir(direction)) {
            Some((tile_map[y][x], (x, y)))
        } else {
            None
        }
    }
}

fn get_new_coord((x, y):(usize, usize), direction:Direction) -> (usize, usize) {
    match direction {
        Direction::North => (x    , y - 1),
        Direction::East  => (x + 1, y),
        Direction::South => (x    , y + 1),
        Direction::West  => (x - 1, y),
    }
}

fn is_pointing(tile:Tile, direction:Direction) -> bool {
    get_directions(tile).contains(&direction)
}

fn get_directions(tile:Tile) -> Vec<Direction> {
    match tile {
        Tile::Vert  => vec![Direction::North, Direction::South],
        Tile::Hor   => vec![Direction::East, Direction::West],
        Tile::NE    => vec![Direction::North, Direction::East],
        Tile::NW    => vec![Direction::North, Direction::West],
        Tile::SW    => vec![Direction::South, Direction::West],
        Tile::SE    => vec![Direction::East, Direction::South],
        Tile::Start => vec![Direction::North, Direction::East, Direction::South, Direction::West],
        Tile::Empty => vec![],
    }
}

fn print_tile_map(tile_map:TileMap) {
    for line in tile_map {
        let mut line_str:String = String::new();
        for tile in line {
            line_str.push_str(tile.to_string().as_str());
        }
        println!("{}", line_str);
    }
}

fn parse_input(input:&str) -> TileMap {
    input.split("\n").map(|line| parse_line(line)).collect()
}

fn parse_line(line:&str) -> Vec<Tile> {
    line.chars().map(|c| parse_tile(c)).collect()
}

fn parse_tile(tile_char:char) -> Tile {
    match tile_char {
        '|' => Tile::Vert,
        '-' => Tile::Hor,
        'L' => Tile::NE,
        'J' => Tile::NW,
        '7' => Tile::SW,
        'F' => Tile::SE,
        '.' => Tile::Empty,
        'S' => Tile::Start,
        _   => panic!("Problem parsing tile!"),
    }
}