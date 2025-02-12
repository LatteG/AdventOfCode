use std::{collections::HashSet, fmt::Debug};

#[derive(Clone, PartialEq, Eq)]
enum Tile {
    Empty,
    Wall,
    Start,
    End,
    Visited(Reindeer)
}

impl Debug for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tile::Empty => write!(f, " "),
            Tile::Wall  => write!(f, "."),
            Tile::Start => write!(f, "S"),
            Tile::End   => write!(f, "E"),
            Tile::Visited(Reindeer { facing: dir, .. }) => write!(f, "{:?}", dir),
        }
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
    fn get_delta_coord(self) -> Coord {
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

impl Debug for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Direction::North => write!(f, "^"),
            Direction::East  => write!(f, ">"),
            Direction::South => write!(f, "v"),
            Direction::West  => write!(f, "<"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Reindeer {
    pos: Coord,
    score: i32,
    facing: Direction,
    path: HashSet<Coord>
}

impl PartialOrd for Reindeer {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Reindeer {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.score.cmp(&other.score)
    }

    fn max(self, other: Self) -> Self
    where
        Self: Sized,
    {
        std::cmp::max_by(self, other, Ord::cmp)
    }

    fn min(self, other: Self) -> Self
    where
        Self: Sized,
    {
        std::cmp::min_by(self, other, Ord::cmp)
    }
}

static  mut PATH_CLONES: usize = 0;

impl Reindeer {
    fn step(self, mut labyrinth: Labyrinth) -> (Vec<Reindeer>, Labyrinth) {
        if is_goal(&self.pos, &labyrinth) {
            (vec![self], labyrinth)
        } else {
            let mut reindeers: Vec<Reindeer> = Vec::new();
            let (x, y): Coord = self.pos;
            for dir in [self.facing.get_counterclockwise(), self.facing, self.facing.get_clockwise()] {
                let (dx, dy): Coord = dir.get_delta_coord();
                let next_pos @ (nx, ny): Coord = (x + dx, y + dy);
                let next_score: i32 = self.score + if dir == self.facing {1} else {1001};

                match labyrinth[ny as usize][nx as usize] {
                    Tile::Wall | Tile::Start => (),
                    Tile::Visited(Reindeer{score: old_score, ..}) if old_score < next_score => (),
                    _ => {
                        let mut next_path: HashSet<Coord> = self.path.clone();
                        next_path.insert(next_pos);
                        reindeers.push(Reindeer{
                            pos: next_pos,
                            score: next_score,
                            facing: dir,
                            path: next_path,
                        })
                    },
                }
            }
            if let Tile::Visited(Reindeer { score: old_score, .. }) = labyrinth[y as usize][x as usize] {
                if old_score > self.score {
                    labyrinth[y as usize][x as usize] = Tile::Visited(self);
                }
            } else {
                labyrinth[y as usize][x as usize] = Tile::Visited(self);
            }
            (reindeers, labyrinth)
        }
    }
}

type Coord = (i32, i32);
type Labyrinth = Vec<Vec<Tile>>;

pub fn task1(input:&str) {
    let (labyrinth, reindeer): (Labyrinth, Reindeer) = parse_input(input);
    let (best_reindeer, _): (Reindeer, Vec<Reindeer>) = run_through_maze(reindeer, labyrinth);
    println!("Lowest score for labyrinth is: {}", best_reindeer.score);
}

pub fn task2(input:&str) {
    let (mut labyrinth, reindeer): (Labyrinth, Reindeer) = parse_input(input);
    let (best_reindeer, reindeers): (Reindeer, Vec<Reindeer>) = run_through_maze(reindeer, labyrinth.clone());
    let end_pos @ (best_x, best_y): Coord = best_reindeer.pos;
    let best_tiles: HashSet<Coord> = find_best_tiles(best_reindeer, reindeers);
    println!("There are {} tiles along the optimal paths", best_tiles.len());
}

fn is_goal((x, y):&Coord, labyrinth:&Labyrinth) -> bool {
    labyrinth[*y as usize][*x as usize] == Tile::End
}

fn print_score(labyrinth:&Labyrinth, (x, y):Coord) {
    if let Tile::Visited(Reindeer { score, .. }) = labyrinth[y as usize][x as usize] {
        println!("Score at ({}, {}): {}", x, y, score);
    }
}

fn find_best_tiles(Reindeer{pos: goal_pos, score: best_score, path: mut coords, ..}:Reindeer, reindeers:Vec<Reindeer>) -> HashSet<Coord> {
    for Reindeer { pos, score, facing: _, path } in reindeers {
        if pos == goal_pos {
            if score == best_score {
                coords = coords.union(&path).copied().collect()
            }
        }
    }
    coords
}

fn run_through_maze(reindeer:Reindeer, mut labyrinth:Labyrinth) -> (Reindeer, Vec<Reindeer>) {//Labyrinth) {
    let mut reindeers: Vec<Reindeer> = vec![reindeer];
    while reindeers.iter().any(|Reindeer {pos, ..}| !is_goal(pos, &labyrinth)) {
        let mut reindeer_acc: Vec<Reindeer> = Vec::new();
        for rd in reindeers {
            let mut temp: Vec<Reindeer> = Vec::new();
            (temp, labyrinth) = rd.step(labyrinth);
            reindeer_acc.append(&mut temp);
        }
        reindeers = reindeer_acc;
    }
    (reindeers.iter().reduce(|min_rd, rd| min_rd.min(rd)).unwrap().clone(), reindeers)
}

fn parse_input(input:&str) -> (Labyrinth, Reindeer) {
    let labyrinth: Labyrinth = input.lines().map(|line| line.chars().map(|tile_char| match tile_char {
        '.' => Tile::Empty,
        '#' => Tile::Wall,
        'S' => Tile::Start,
        'E' => Tile::End,
         _  => panic!("Unrecognized tile: {}", tile_char)
    }).collect()).collect();

    let y: i32 = labyrinth.iter().position(|line| line.contains(&Tile::Start)).unwrap() as i32;
    let x: i32 = labyrinth[y as usize].iter().position(|tile| *tile == Tile::Start).unwrap() as i32;

    let reindeer: Reindeer = Reindeer{
        pos: (x, y),
        score: 0,
        facing: Direction::East,
        path: vec![(x, y)].into_iter().collect(),
    };

    (labyrinth, reindeer)
}
