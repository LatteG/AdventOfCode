use std::{cmp::{max, min}, iter};


type Coord = (usize, usize);
type RaceTrack = Vec<Coord>;

pub fn task1(input:&str) {
    let track: RaceTrack = parse_input(input);
    let min_improvement: usize = if track.len() > 200 {100} else {20};
    let cheat_count: u32 = find_cheat_count(track, min_improvement);
    println!("There are {} cheats that improve the time by at least {} picoseconds", cheat_count, min_improvement);
}

pub fn task2(input:&str) {
    println!("TODO: Task 2")
}

fn find_cheat_count(track:RaceTrack, min_improvement:usize) -> u32 {
    let mut cheat_count: u32 = 0;
    for start_index in 0..(track.len() - min_improvement) {
        let start @ (sx, sy): Coord = track[start_index];
        for end_index in (start_index + min_improvement)..track.len() {
            let end @ (ex, ey): Coord = track[end_index];
            let dx: usize = sx.abs_diff(ex);
            let dy: usize = sy.abs_diff(ey);
            if let Some(walls_phased) = check_valid_cheat(&track, start, end) {
                if end_index - start_index - walls_phased - 1 >= min_improvement {
                    cheat_count += 1;
                    // println!("({:2}, {:2}) -> ({:2}, {:2}), saving: {}", sx, sy, ex, ey, end_index - start_index - walls_phased - 1);
                }
            }
        }
    }
    cheat_count
}

fn check_valid_cheat(track:&RaceTrack, start@(sx,sy):Coord, end@(ex,ey):Coord) -> Option<usize> {
    let dx: usize = sx.abs_diff(ex);
    let dy: usize = sy.abs_diff(ey);

    let min_x: usize = min(sx, ex);
    let max_x: usize = max(sx, ex);
    let min_y: usize = min(sy, ey);
    let max_y: usize = max(sy, ey);

    if dx + dy > 3 || dx + dy < 2 {
        None
    } else {
        let mut empty_count: u32 = 0;
        for cx in min_x..=max_x {
            for cy in min_y..=max_y {
                if [start, end].contains(&(cx, cy)) {
                    continue;
                }
                if track.contains(&(cx, cy)) {
                    return None;
                }
            }
        }
        Some(dx + dy - 1)
    }
}

/*
.#
#.

#.
.#
*/

fn parse_input(input:&str) -> RaceTrack {
    let track_map: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();
    
    let start_y: usize = track_map.iter().position(|line| line.contains(&'S')).unwrap();
    let start_x: usize = track_map[start_y].iter().position(|c| c == &'S').unwrap();

    let mut track: RaceTrack = vec![(start_x, start_y)];
    let mut x: usize = start_x;
    let mut y: usize = start_y;
    while track_map[y][x] != 'E' {
        let mut found_next:bool = false;
        for next @ (nx, ny) in [(x-1,y), (x, y-1), (x+1, y), (x, y+1)] {
            if track.contains(&next) {
                continue;
            }
            if ['.', 'E'].contains(&track_map[ny][nx]) {
                track.push(next);
                x = nx;
                y = ny;
                found_next = true;
                break;
            }
        }
        if !found_next {
            panic!("Did not find next after ({}, {})!", x, y);
        }
    }

    track
}
