use std::fmt::Debug;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Tile {
    Empty,
    Wall,
    Start,
    End,
    Visited
}

impl Debug for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tile::Empty   => write!(f, "."),
            Tile::Wall    => write!(f, "#"),
            Tile::Start   => write!(f, "S"),
            Tile::End     => write!(f, "E"),
            Tile::Visited => write!(f, "O"),
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

#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd)]
struct Reindeer {
    pos: Coord,
    score: i32,
    facing: Direction
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

impl Reindeer {
    fn step(self, mut labyrinth: Labyrinth) -> (Vec<Reindeer>, Labyrinth) {
        if self.has_finished(&labyrinth) {
            (vec![self], labyrinth)
        } else {
            let mut reindeers: Vec<Reindeer> = Vec::new();
            for dir in [self.facing.get_counterclockwise(), self.facing, self.facing.get_clockwise()] {
                let (dx, dy): Coord = dir.get_delta_coord();
                if vec![Tile::Empty, Tile::End].contains(&labyrinth[(self.pos.1 + dy) as usize][(self.pos.0 + dx) as usize]) {
                    reindeers.push(Reindeer{
                        pos: (self.pos.0 + dx, self.pos.1 + dy),
                        score: self.score + if dir == self.facing {1} else {1001},
                        facing: dir,
                    });
                }
            }
            labyrinth[self.pos.1 as usize][self.pos.0 as usize] = Tile::Visited;
            (reindeers, labyrinth)
        }
    }

    fn has_finished(self, labyrinth: &Labyrinth) -> bool {
        labyrinth[self.pos.1 as usize][self.pos.0 as usize] == Tile::End
    }
}

type Coord = (i32, i32);
type Labyrinth = Vec<Vec<Tile>>;

pub fn task1(input:&str) {
    let (labyrinth, reindeer): (Labyrinth, Reindeer) = parse_input(input);
    // for line in &labyrinth {
    //     println!("{:?}", line);
    // }
    // println!("{:?}", reindeer);
    let best_reindeer: Reindeer = run_through_maze(reindeer, labyrinth);
    println!("Lowest score for labyrinth is: {}", best_reindeer.score);
}

pub fn task2(input:&str) {
    println!("TODO: Task 2")
}

fn run_through_maze(reindeer:Reindeer, mut labyrinth:Labyrinth) -> Reindeer {
    let mut reindeers: Vec<Reindeer> = vec![reindeer];
    while reindeers.iter().any(|rd| !rd.has_finished(&labyrinth)) {
        let mut reindeer_acc: Vec<Reindeer> = Vec::new();
        for rd in reindeers {
            let mut temp: Vec<Reindeer> = Vec::new();
            (temp, labyrinth) = rd.step(labyrinth);
            reindeer_acc.append(&mut temp);
        }
        reindeers = reindeer_acc.clone();
        // for line in &labyrinth {
        //     println!("{:?}", line)
        // }
        // println!("{:?}", reindeers)
    }
    *reindeers.iter().reduce(|min_rd, rd| min_rd.min(rd)).unwrap()
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
    };

    (labyrinth, reindeer)
}
