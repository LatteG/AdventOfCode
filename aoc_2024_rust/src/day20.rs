use std::{cmp::{max, min}, path::Iter};

use memoize::memoize;


type Coord = (usize, usize);
type RaceTrack = Vec<Coord>;

pub fn task1(input:&str) {
    let track: RaceTrack = parse_input(input);
    println!("Got the track!");
    let min_improvement: usize = 20;
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
            if dx + dy <= 3 && check_valid_cheat(track.clone(), start, end) {
                cheat_count += 1;
                println!("({:2}, {:2}) -> ({:2}, {:2}), saving: {}", sx, sy, ex, ey, end_index - start_index - 1);
            }
        }
    }
    cheat_count
}

#[memoize]
fn check_valid_cheat(track:RaceTrack, start@(sx,sy):Coord, end@(ex,ey):Coord) -> bool {
    let dx: usize = sx.abs_diff(ex);
    let dy: usize = sy.abs_diff(ey);

    let min_x: usize = min(sx, ex);
    let max_x: usize = max(sx, ex);
    let min_y: usize = min(sy, ey);
    let max_y: usize = max(sy, ey);

    if dx + dy > 3 {
        false
    } else {
        if dy == 0 {
            for x in (min_x + 1)..max_x {
                if track.contains(&(sy, x)) {
                    return false
                }
            }
            true
        } else if dx == 0 {
            for y in (min_y + 1)..max_y {
                if track.contains(&(y, sx)) {
                    return false;
                }
            }
            true
        } else if dy == 1 && dx == 1 {
            let neighbours: Vec<Coord> = vec![(0, 0), (0, 1), (1, 0), (1, 1)];
            neighbours.iter().fold(0, |acc, (x_offset, y_offset):&Coord| if track.contains(&(min_x + x_offset, min_y + x_offset)) {acc + 1} else {acc}) == 2
        } else {
            let mut neighbours: Vec<Coord> = Vec::new();
            let mut temp: Coord = (0, 0);
            if sx > min_x {
                temp = (sx - 1, sy);
                if !track.contains(&temp) {
                    neighbours.push(temp);
                }
            } else if sx < max_x {
                temp = (sx + 1, sy);
                if !track.contains(&temp) {
                    neighbours.push(temp);
                }
            }
            if sy > min_y {
                temp = (sx, sy - 1);
                if !track.contains(&temp) {
                    neighbours.push(temp);
                }
            } else if sy < max_y {
                temp = (sx, sy + 1);
                if !track.contains(&temp) {
                    neighbours.push(temp);
                }
            }
            neighbours.iter().any(|next_start| check_valid_cheat(track.clone(), *next_start, end))
        }
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
