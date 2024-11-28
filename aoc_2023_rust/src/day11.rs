use std::cmp::{max, min};

use array2d::Array2D;

type Space = Array2D<bool>;
type Coord = (usize, usize);

pub fn task1(input:&str) {
    let space:Space = parse_input(input);
    let expanded_space:Space = expand_rows(&expand_cols(&space.clone()));
    let galaxy_coords:Vec<Coord> = get_galaxy_coords(&expanded_space);
    let galaxy_distances:Vec<u64> = get_galaxy_distances(galaxy_coords.clone());
    println!("Sum of all galaxy distances: {}", galaxy_distances.iter().sum::<u64>());
}

pub fn task2(input:&str) {
    let expansion_factor:u64 = 1_000_000;
    let space:Space = parse_input(input);
    let expanded_rows:Vec<usize> = get_expanded_row_indices(&space);
    let expanded_cols:Vec<usize> = get_expanded_col_indices(&space);
    let galaxy_coords:Vec<Coord> = get_galaxy_coords(&space);
    let galaxy_distances:Vec<u64> = get_big_galaxy_distances(galaxy_coords.clone(), &expanded_rows, &expanded_cols, expansion_factor);
    println!("Sum of all galaxy distances with an expansion factor of {}: {}", expansion_factor, galaxy_distances.iter().sum::<u64>());
}

fn get_big_galaxy_distances(mut galaxy_coords:Vec<Coord>, expanded_rows:&Vec<usize>, expanded_cols:&Vec<usize>, expansion_factor:u64) -> Vec<u64> {
    let mut distances:Vec<u64> = Vec::new();
    while galaxy_coords.len() > 1 {
        let curr:Coord = galaxy_coords.pop().unwrap();
        for other in galaxy_coords.clone() {
            distances.push(get_big_distance(curr, other, expanded_rows, expanded_cols, expansion_factor));
        }
    }
    distances
}

fn get_galaxy_distances(mut galaxy_coords:Vec<Coord>) -> Vec<u64> {
    let mut distances:Vec<u64> = Vec::new();
    while galaxy_coords.len() > 1 {
        let curr:Coord = galaxy_coords.pop().unwrap();
        for other in galaxy_coords.clone() {
            distances.push(get_distance(curr, other));
        }
    }
    distances
}

fn get_big_distance((x0, y0):Coord, (x1, y1):Coord, x_expansion:&Vec<usize>, y_expansion:&Vec<usize>, expansion_factor:u64) -> u64 {
    calc_dist(x0, x1, x_expansion, expansion_factor) + calc_dist(y0, y1, y_expansion, expansion_factor)
}

fn calc_dist(v0:usize, v1:usize, expansion_indices:&Vec<usize>, expansion_factor:u64) -> u64 {
    let max_v:usize = max(v0, v1);
    let min_v:usize = min(v0, v1);
    let v_diff:usize = max_v - min_v;
    let expansion_point_amount:usize = expansion_indices.iter().filter(|i| **i > min_v && **i < max_v).collect::<Vec<_>>().len();
    (v_diff - expansion_point_amount) as u64 + (expansion_point_amount as u64) * expansion_factor
}

fn get_distance((x0, y0):Coord, (x1, y1):Coord) -> u64 {
    (x0 as u64).abs_diff(x1 as u64) + (y0 as u64).abs_diff(y1 as u64)
}

fn get_galaxy_coords(space:&Space) -> Vec<Coord> {
    let mut galaxy_coords:Vec<Coord> = Vec::new();
    for (coord, is_galaxy) in space.enumerate_row_major() {
        if *is_galaxy {
            galaxy_coords.push(coord);
        }
    }
    galaxy_coords
}

fn print_space(space:&Space) {
    for row in space.rows_iter() {
        for elem in row {
            match elem {
                true => print!("#"),
                false => print!("."),
            }
        }
        print!("\n");
    }
}

fn get_expanded_row_indices(space:&Space) -> Vec<usize> {
    let mut expanded_row_indices:Vec<usize> = Vec::new();
    for (row, index) in space.rows_iter().zip(0..space.num_rows()) {
        if row.into_iter().all(|e| !*e) {
            expanded_row_indices.push(index);
        }
    }
    expanded_row_indices
}

fn get_expanded_col_indices(space:&Space) -> Vec<usize> {
    let mut expanded_col_indices:Vec<usize> = Vec::new();
    for (col, index) in space.columns_iter().zip(0..space.num_columns()) {
        if col.into_iter().all(|e| !*e) {
            expanded_col_indices.push(index);
        }
    }
    expanded_col_indices
}

fn expand_cols(small_space:&Space) -> Space {
    let mut expanded_cols:Vec<Vec<bool>> = Vec::new();
    for col in small_space.as_columns() {
        expanded_cols.push(col.clone());
        if col.iter().all(|e| !*e) {
            expanded_cols.push(col.clone());
        }
    }
    match Array2D::from_columns(&expanded_cols) {
        Ok(space) => space,
        Err(..) => panic!("Problem expanding columns!"),
    }
}

fn expand_rows(small_space:&Space) -> Space {
    let mut expanded_rows:Vec<Vec<bool>> = Vec::new();
    for row in small_space.as_rows() {
        expanded_rows.push(row.clone());
        if row.iter().all(|e| !*e) {
            expanded_rows.push(row.clone());
        }
    }
    match Array2D::from_rows(&expanded_rows) {
        Ok(space) => space,
        Err(..) => panic!("Problem expanding rows!"),
    }
}

fn parse_input(input:&str) -> Space {
    match Array2D::from_rows(&input.split_whitespace().map(|l| parse_line(l)).collect::<Vec<Vec<bool>>>()) {
        Ok(space) => space,
        Err(..) => panic!("Problem parsing space!"),
    }
}

fn parse_line(line:&str) -> Vec<bool> {
    line.chars().map(|c| c == '#').collect()
}