#[derive(Clone)]
struct Robot {
    position: Coord,
    velocity: Coord,
}

impl Robot {
    fn calc_pos(&self, time_seconds: i32, space_size @ (width, height): Coord) -> Coord {
        let new_x: i32 = self.position.0 + self.velocity.0 * time_seconds;
        let new_y: i32 = self.position.1 + self.velocity.1 * time_seconds;
        let new_pos: Coord = (new_x.rem_euclid(width), new_y.rem_euclid(height));
        new_pos
    }
}

type Coord = (i32, i32);

const TEST_SIZE: Coord = (11, 7);
const REAL_SIZE: Coord = (101, 103);

pub fn task1(input:&str) {
    let (robots, space_size): (Vec<Robot>, Coord) = parse_input(input);
    let robot_positions: Vec<Coord> = robots.iter().map(|robot| robot.calc_pos(100, space_size)).collect();
    let (q0_count, q1_count, q2_count, q3_count): (i32, i32, i32, i32) = get_quadrant_count(robot_positions, space_size);
    println!("The safety factor is: {}", q0_count * q1_count * q2_count * q3_count);
}

pub fn task2(input:&str) {
    let (robots, space_size): (Vec<Robot>, Coord) = parse_input(input);
    for seconds in 1..(space_size.0 * space_size.1) {
        let positions: Vec<Coord> = robots.iter().map(|robot| robot.calc_pos(seconds, space_size)).collect();
        let structure: String = get_robot_structure(positions, space_size);
        if structure.contains("#############################") {
            println!("You get a Christmas tree after {} seconds\n{}", seconds, structure);
            break;
        }
    }
}

fn get_robot_structure(robot_positions:Vec<Coord>, space_size @ (width, height):Coord) -> String {
    let mut canvas: Vec<Vec<&str>> = vec![vec!["."; width as usize]; height as usize];
    for (x, y) in robot_positions {
        canvas[y as usize][x as usize] = "#";
    }
    let lines: Vec<String> = canvas.iter().map(|line| line.join("")).collect();
    lines.join("\n")
}

fn get_quadrant_count(positions:Vec<Coord>, space_size @ (width, height):Coord) -> (i32, i32, i32, i32) {
    let (mut q0, mut q1, mut q2, mut q3): (i32, i32, i32, i32) = (0, 0, 0, 0);
    let w_mid: i32 = width / 2;
    let h_mid: i32 = height / 2;

    for (x, y) in positions {
        if x < w_mid && y < h_mid {
            q0 += 1;
        } else if x < w_mid && y > h_mid {
            q1 += 1;
        } else if x > w_mid && y < h_mid {
            q2 += 1;
        } else if x > w_mid && y > h_mid {
            q3 += 1;
        }
    }

    (q0, q1, q2, q3)
}

fn parse_input(input:&str) -> (Vec<Robot>, Coord) {
    let lines: Vec<&str> = input.split("\n").collect();
    let robots: Vec<Robot> = lines.iter().map(|line| parse_robot(line)).collect();
    if robots.len() > 20 {
        (robots, REAL_SIZE)
    } else {
        (robots, TEST_SIZE)
    }
}

fn parse_robot(robot_str: &str) -> Robot {
    let (pos_str, vel_str): (&str, &str) = robot_str.split_once(" ").unwrap();
    Robot{
        position: parse_coord(pos_str),
        velocity: parse_coord(vel_str)
    }
}

fn parse_coord(coord_str:&str) -> Coord {
    let (x_str, y_str): (&str, &str) = coord_str.split_once("=").unwrap().1.split_once(",").unwrap();
    (parse_num(x_str), parse_num(y_str))
}

fn parse_num(num_str:&str) -> i32 {
    match num_str.parse::<i32>() {
        Ok(num) => num,
        Err(_) => panic!("Problem parsing number"),
    }
}
