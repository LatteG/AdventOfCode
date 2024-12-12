use std::collections::{HashMap, HashSet};

#[derive(Clone)]
struct Region {
    plots: Vec<Coord>,
    plant: char
}

impl Region {
    fn print(&self) {
        println!("Plant: {}\nPlots: {:?}", self.plant, self.plots);
    }

    fn get_area(&self) -> u32 {
        self.plots.len() as u32
    }

    fn get_shape(&self) -> Vec<Vec<bool>> {
        let (xs, ys): (Vec<i32>, Vec<i32>) = self.plots.clone().into_iter().unzip();
        let min_x: i32 = *xs.iter().min().unwrap();
        let max_x: i32 = *xs.iter().max().unwrap();
        let min_y: i32 = *ys.iter().min().unwrap();
        let max_y: i32 = *ys.iter().max().unwrap();
        let mut shape: Vec<Vec<bool>> = vec![vec![false; (max_x - min_x + 1) as usize]; (max_y - min_y + 1) as usize];
        for (x, y) in self.plots.clone() {
            shape[(y - min_y) as usize][(x - min_x) as usize] = true;
        }

        shape
    }

    fn get_perimeter(&self) -> u32 {
        let mut perimeter: u32 = 0;
        let shape: Vec<Vec<bool>> = self.get_shape();
        for y in 0..shape.len() {
            for x in 0..shape[0].len() {
                if shape[y][x] {
                    for (nx, ny) in get_neighbours((x as i32, y as i32)) {
                        if 0 <= nx && nx < shape[0].len() as i32 && 0 <= ny && ny < shape.len() as i32 {
                            if !shape[ny as usize][nx as usize] {
                                perimeter += 1;
                            }
                        } else {
                            perimeter += 1;
                        }
                    }
                }
            }
        }
        perimeter
    }

    fn get_number_of_sides(&self) -> u32 {
        let mut sides: u32 = 0;
        let shape: Vec<Vec<bool>> = self.get_shape();
        for y in 0..shape.len() {
            for x in 0..shape[0].len() {
                if shape[y][x] {
                    let x_neg_in:bool = 0 <= x as i32 - 1;
                    let y_neg_in:bool = 0 <= y as i32 - 1;
                    let x_pos_in:bool = x + 1 < shape[0].len();
                    let y_pos_in:bool = y + 1 < shape.len();

                    // Check left
                    if !x_neg_in || !shape[y][x-1] {
                        if !y_neg_in || !shape[y-1][x] || y_neg_in && x_neg_in && shape[y-1][x-1] {
                            sides += 1
                        }
                    }

                    // Check top
                    if !y_neg_in || !shape[y-1][x] {
                        if !x_neg_in || !shape[y][x-1] || x_neg_in && y_neg_in && shape[y-1][x-1] {
                            sides += 1
                        }
                    }

                    // Check right
                    if !x_pos_in || !shape[y][x+1] {
                        if !y_pos_in || !shape[y+1][x] || y_pos_in && x_pos_in && shape[y+1][x+1] {
                            sides += 1
                        }
                    }

                    // Check bot
                    if !y_pos_in || !shape[y+1][x] {
                        if !x_pos_in || !shape[y][x+1] || x_pos_in && y_pos_in && shape[y+1][x+1] {
                            sides += 1
                        }
                    }
                }
            }
        }
        sides
    }

    fn get_price(&self) -> u32 {
        let price: u32 = self.get_area() * self.get_perimeter();
        price
    }

    fn get_discount_price(&self) -> u32 {
        let price: u32 = self.get_area() * self.get_number_of_sides();
        price
    }
}

type Coord = (i32, i32);
type PlantMap = HashMap<Coord, char>;

pub fn task1(input:&str) {
    let plant_map: PlantMap = parse_input(input);
    let regions: Vec<Region> = find_regions(plant_map.clone());
    let price: u32 = regions.iter().fold(0, |acc, region| acc + region.get_price());
    
    println!("Total fence cost: {}", price)
}

pub fn task2(input:&str) {
    let plant_map: PlantMap = parse_input(input);
    let regions: Vec<Region> = find_regions(plant_map.clone());
    let price: u32 = regions.iter().fold(0, |acc, region| acc + region.get_discount_price());
    
    println!("Total fence discounted cost: {}", price)
}

fn find_regions(mut plant_map:PlantMap) -> Vec<Region> {
    let mut regions: Vec<Region> = Vec::new();
    let mut plant: char = plant_map[&(0, 0)];
    let mut neighbour_plants: HashSet<Coord> = HashSet::from([(0, 0)]);
    let mut found_coords: Vec<Coord> = Vec::new();

    loop {
        for c in neighbour_plants.clone() {
            found_coords.push(c);
            plant_map.remove(&c);
        }
        
        // Get the next set of coords
        neighbour_plants = neighbour_plants.into_iter().flat_map(|c| get_neighbours(c)).filter(|c| !found_coords.contains(c) && plant_map.contains_key(&c) && plant_map[&c] == plant).collect();

        // Save region and start a new one when there are no more coords to check
        if neighbour_plants.len() == 0 {
            regions.push(Region{plots: found_coords, plant: plant});
            match plant_map.keys().last() {
                Some(next_start_coord) => {
                    neighbour_plants.insert(*next_start_coord);
                    plant = plant_map[next_start_coord];
                    found_coords = Vec::new();
                },
                None => return regions,
            }
        }
    }

    regions
}

fn get_neighbours((x, y):Coord) -> HashSet<Coord> {
    HashSet::from([(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)])
}

fn parse_input(input:&str) -> PlantMap {
    let input_map: Vec<Vec<char>> = input.split_whitespace().map(|line| line.chars().collect()).collect();
    let mut return_map: PlantMap = HashMap::new();
    for x in 0..input_map[0].len() {
        for y in 0..input_map.len() {
            return_map.insert((x as i32, y as i32), input_map[y][x]);
        }
    }
    return_map
}
