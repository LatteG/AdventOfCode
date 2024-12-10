use std::collections::{HashMap, HashSet};

type Coord = (i32, i32);

pub fn task1(input:&str) {
    let (antenna_map, x_len, y_len): (HashMap<char, Vec<Coord>>, usize, usize) = parse_input(input);
    let antinodes: HashSet<Coord> = find_all_antinodes(antenna_map.clone(), x_len, y_len, false);
    println!("There are {} antinodes without resonance", antinodes.len());
    // print_map(antenna_map, antinodes, x_len, y_len);
}

pub fn task2(input:&str) {
    let (antenna_map, x_len, y_len): (HashMap<char, Vec<Coord>>, usize, usize) = parse_input(input);
    let antinodes: HashSet<Coord> = find_all_antinodes(antenna_map.clone(), x_len, y_len, true);
    println!("There are {} antinodes with resonance", antinodes.len());
    // print_map(antenna_map, antinodes, x_len, y_len);
}

fn print_map(antenna_map:HashMap<char, Vec<Coord>>, antinodes:HashSet<Coord>, x_len:usize, y_len:usize) {
    let mut characters: Vec<Vec<char>> = vec![vec!['.'; x_len]; y_len];
    for (x, y) in antinodes {
        characters[y as usize][x as usize] = '#';
    }

    for (c, coords) in antenna_map {
        for (x, y) in coords {
            characters[y as usize][x as usize] = c;
        }
    }

    let lines: Vec<String> = characters.iter().map(|line_chars| line_chars.iter().collect()).collect();
    let map_str: String = lines.join("\n");

    println!("{}", map_str);
}

fn find_all_antinodes(antenna_map:HashMap<char, Vec<Coord>>, x_len:usize, y_len:usize, resonance:bool) -> HashSet<Coord> {
    let antenna_lists: Vec<&Vec<Coord>> = antenna_map.values().collect();
    antenna_lists.into_iter().map(|antennas| find_antinodes(antennas, x_len, y_len, resonance)).flatten().collect()
}

fn find_antinodes(antennas:&Vec<Coord>, x_len:usize, y_len:usize, resonance:bool) -> HashSet<Coord> {
    let mut antinodes: HashSet<Coord> = HashSet::new();

    for a0 @ (x0, y0) in antennas {
        for a1 @ (x1, y1) in antennas {
            if a0 == a1 {
                continue;
            }

            let mut dx: i32 = x0 - x1;
            let mut dy: i32 = y0 - y1;

            if resonance {
                for i in 2..dx {
                    if dx % i == 0 && dy % i == 0 {
                        dx /= i;
                        dy /= i;
                    }
                }
            }

            let mut x2: i32 = if resonance {*x0} else {x0 + dx};
            let mut y2: i32 = if resonance {*y0} else {y0 + dy};
            
            while 0 <= x2 && x2 < x_len as i32 && 0 <= y2 && y2 < y_len as i32 {
                antinodes.insert((x2, y2));
                if !resonance {
                    break;
                }
                x2 += dx;
                y2 += dy;
            }
        }
    }

    antinodes
}

fn parse_input(input:&str) -> (HashMap<char, Vec<Coord>>, usize, usize) {
    let char_map: Vec<Vec<char>> = input.split("\n").map(|line| line.chars().collect()).collect();
    let mut antenna_map: HashMap<char, Vec<Coord>> = HashMap::new();
    for y in 0..char_map.len() {
        for x in 0..char_map[0].len() {
            let c: char = char_map[y][x];
            if c != '.' {
                if !antenna_map.contains_key(&c) {
                    antenna_map.insert(c, Vec::new());
                }
                antenna_map.get_mut(&c).unwrap().push((x as i32, y as i32));
            }
        }
    }
    (antenna_map, char_map[0].len(), char_map.len())
}
