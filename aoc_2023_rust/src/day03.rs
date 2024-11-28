use std::cmp::{max, min};

struct PartNumber {
    number: i32,
    min_x: i32,
    max_x: i32,
    y: i32,
}

enum SchematicSymbol {
    Empty,
    Symbol,
    Number,
    Gear,
}

struct Schematic {
    width: i32,
    height: i32,
    schematic: Vec<Vec<SchematicSymbol>>,
    part_numbers: Vec<PartNumber>,
    gear_locations: Vec<(i32, i32)>,
}

impl PartNumber {
    fn to_string(&self) -> String {
        format!("{} ({}-{}, {})", self.number, self.min_x, self.max_x, self.y)
    }
    fn get_neighbour_coords(&self, width:i32, height:i32) -> Vec<(i32, i32)> {
        let mut neighbours:Vec<(i32, i32)> = Vec::new();
        for x in max(0, self.min_x - 1)..min(width, self.max_x + 2) {
            for y in max(0, self.y - 1)..min(height, self.y + 2) {
                if x < self.min_x || x > self.max_x || y != self.y {
                    neighbours.push((x, y));
                }
            }
        }
        return neighbours;
    }
    fn is_neighbour(&self, (x, y):(i32, i32)) -> bool {
        x >= self.min_x - 1 && x <= self.max_x + 1 && (y - self.y).abs() <= 1
    }
}

impl Schematic {
    fn to_string(&self) -> String {
        let part_number_strings: Vec<String> = self.part_numbers.iter().map(|pn| pn.to_string()).collect();
        let schematic_strings: Vec<String> = self.schematic.iter().map(|l| l.iter().map(|ss| schematic_symbol_to_char(ss)).collect()).collect();
        let schematic_string: String = schematic_strings.iter().fold("".to_string(), |acc, s| format!("{}\n{}", acc, s));
        format!("width: {}\nheight: {}\npart numbers:\n{:?}\nschematic:\n{}", self.width, self.height, part_number_strings, schematic_string.trim())
    }
    fn get_valid_part_numbers(&self) -> Vec<i32> {
        let mut valid_part_numbers:Vec<i32> = Vec::new();
        for pn in self.part_numbers.as_slice() {
            if pn.get_neighbour_coords(self.width, self.height).iter().any(|c| self.is_symbol_at_coord(*c)) {
                valid_part_numbers.push(pn.number);
            }
        }
        return valid_part_numbers;
    }
    fn is_symbol_at_coord(&self, (x, y):(i32, i32)) -> bool {
        match self.schematic[y as usize][x as usize] {
            SchematicSymbol::Symbol => true,
            SchematicSymbol::Gear   => true,
            _ => false,
        }
    }
    fn get_gear_ratios(&self) -> Vec<i32> {
        let mut gear_ratios:Vec<i32> = Vec::new();
        for gear in self.gear_locations.as_slice() {
            let neighbours:Vec<&PartNumber> = self.part_numbers.iter().filter(|pn| pn.is_neighbour(*gear)).collect();
            if neighbours.len() == 2 {
                gear_ratios.push(neighbours[0].number * neighbours[1].number);
            }
        }
        return gear_ratios;
    }
}

pub fn task1(input:&str) {
    let schem:Schematic = parse_input(input);
    let valid_part_numbers:Vec<i32> = schem.get_valid_part_numbers();
    println!("Valid part number total: {}", valid_part_numbers.iter().sum::<i32>());
}

pub fn task2(input:&str) {
    let schem:Schematic = parse_input(input);
    let gear_ratios:Vec<i32> = schem.get_gear_ratios();
    println!("Gear ratio sum: {:?}", gear_ratios.iter().sum::<i32>());
}

fn parse_input(input:&str) -> Schematic {
    let chars:Vec<Vec<char>> = input.split_whitespace().map(|l| l.chars().collect()).collect();
    
    let width:i32 = chars.first().unwrap().len().try_into().unwrap();
    let height:i32 = chars.len().try_into().unwrap();

    let schematic:Vec<Vec<SchematicSymbol>> = chars.iter().map(|l| l.iter().map(|c| char_to_schematic_symbol(c)).collect()).collect();

    let mut part_numbers: Vec<PartNumber> = Vec::new();
    let mut gear_locations: Vec<(i32, i32)> = Vec::new();
    for y in 0..height as usize {
        let mut acc:i32 = 0;
        let mut start_x:i32 = 0;
        for x in 0..width as usize {
            if chars[y][x].is_digit(10) {
                if acc == 0 {
                    start_x = x as i32;
                }
                acc = 10 * acc + chars[y][x].to_digit(10).unwrap() as i32;
            }
            if (!chars[y][x].is_digit(10) || x == width as usize -1) && acc > 0 {
                part_numbers.push(PartNumber { 
                    number: acc, 
                    min_x: start_x, 
                    max_x: x as i32 - 1, 
                    y: y as i32
                });
                acc = 0;
            }
            if chars[y][x] == '*' {
                gear_locations.push((x as i32, y as i32));
            }
        }
    }

    Schematic{
        width: width,
        height: height,
        schematic: schematic,
        part_numbers: part_numbers,
        gear_locations: gear_locations,
    }
}

fn char_to_schematic_symbol(c:&char) -> SchematicSymbol {
    if c == &'.' {
        SchematicSymbol::Empty
    } else if c.is_digit(10) {
        SchematicSymbol::Number
    } else if c == &'*' {
        SchematicSymbol::Gear
    }else {
        SchematicSymbol::Symbol
    }
}
fn schematic_symbol_to_char(ss: &SchematicSymbol) -> char {
    match ss {
        SchematicSymbol::Empty  => ' ',
        SchematicSymbol::Number => '#',
        SchematicSymbol::Symbol => '_',
        SchematicSymbol::Gear   => '*'
    }
}