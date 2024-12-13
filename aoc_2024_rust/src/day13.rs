struct ClawMachine {
    a_button: Coord,
    b_button: Coord,
    prize: Coord,
}

impl ClawMachine {
    fn get_prize(&self) -> Option<u64> {
        for a in 0..=100 as usize {
            for b in 0..=100 as usize {
                let claw_location: Coord = (a * self.a_button.0 + b * self.b_button.0, a * self.a_button.1 + b * self.b_button.1);
                if claw_location == self.prize {
                    return Some((a * 3 + b) as u64);
                } else if claw_location.0 > self.prize.0 || claw_location.0 > self.prize.0 {
                    if b == 0 {
                        return None;
                    } else {
                        break;
                    }
                }
            }
        }
        return None;
    }

    fn get_prize_big_offset(&self) -> Option<u64> {
        let offset: usize = 10000000000000;
        let prize_location @ (prize_x, prize_y): Coord = (self.prize.0 + offset, self.prize.1 + offset);

        let a_slope: f64 = self.a_button.1 as f64 / self.a_button.0 as f64;
        let b_slope: f64 = self.b_button.1 as f64 / self.b_button.0 as f64;

        // Check if it is even worth trying
        if (prize_y as f64) < (a_slope * prize_x as f64).min(b_slope * prize_x as f64) || (prize_y as f64) > (a_slope * prize_x as f64).max(b_slope * prize_x as f64) {
            return None;
        }

        let prize_slope: f64 = prize_y as f64 / prize_x as f64;

        let shallower_button @ ((shallower_x, shallower_y), shallower_cost): (Coord, u64) = if a_slope < b_slope {(self.a_button, 3)} else {(self.b_button, 1)};
        let steeper_button @ ((steeper_x, steeper_y), steeper_cost): (Coord, u64) = if a_slope > b_slope {(self.a_button, 3)} else {(self.b_button, 1)};

        let mut claw_pos: Coord = self.b_button;
        let mut acc_cost: u64 = 1;

        while claw_pos.0 < prize_x && claw_pos.1 < prize_y {
            let step_size: usize = (((prize_x - claw_pos.0) / shallower_x.max(steeper_x)).min((prize_y - claw_pos.1) / shallower_y.max(steeper_y)) / 10).max(1);
            if (claw_pos.1 as f64) < prize_slope * claw_pos.0 as f64 {
                claw_pos = (claw_pos.0 + steeper_x * step_size, claw_pos.1 + steeper_y * step_size);
                acc_cost += steeper_cost * step_size as u64;
            } else {
                claw_pos = (claw_pos.0 + shallower_x * step_size, claw_pos.1 + shallower_y * step_size);
                acc_cost += shallower_cost * step_size as u64;
            }
            if claw_pos == prize_location {
                return Some(acc_cost);
            }
        }

        return None;
    }
}

type Coord = (usize, usize);

pub fn task1(input:&str) {
    let claw_machines: Vec<ClawMachine> = parse_input(input);
    let prize_costs: Vec<Option<u64>> = claw_machines.iter().map(|cm| cm.get_prize()).collect();
    let token_amount: u64 = prize_costs.iter().fold(0, |acc, maybe_cost| match maybe_cost {
        Some(cost) => acc + cost,
        None => acc,
    });
    println!("Total cost for all possible prizes: {}", token_amount);
}

pub fn task2(input:&str) {
    let claw_machines: Vec<ClawMachine> = parse_input(input);
    let prize_costs: Vec<Option<u64>> = claw_machines.iter().map(|cm| cm.get_prize_big_offset()).collect();
    let token_amount: u64 = prize_costs.iter().fold(0, |acc, maybe_cost| match maybe_cost {
        Some(cost) => acc + cost,
        None => acc,
    });
    println!("Total cost for all possible prizes with the big offset: {}", token_amount);
}

fn parse_input(input:&str) -> Vec<ClawMachine> {
    let claw_machine_strings: Vec<&str> = input.split("\n\n").collect();
    claw_machine_strings.iter().map(|cm_str| parse_claw_machine(cm_str)).collect()
}

fn parse_claw_machine(cm_str:&str) -> ClawMachine {
    let lines: Vec<&str> = cm_str.split("\n").collect();

    let a_btn_str: &str = lines[0].split_once(": ").unwrap().1;
    let b_btn_str: &str = lines[1].split_once(": ").unwrap().1;
    let prize_str: &str = lines[2].split_once(": ").unwrap().1;
    
    ClawMachine {
        a_button: parse_coord(a_btn_str, "+"),
        b_button: parse_coord(b_btn_str, "+"),
        prize: parse_coord(prize_str, "=")
    }
}

fn parse_coord(coord_str:&str, label_delim:&str) -> Coord {
    let (x_str, y_str) = coord_str.split_once(", ").unwrap();
    let parse_num = |s:&str| s.split_once(label_delim).unwrap().1.parse::<usize>().unwrap();
    (parse_num(x_str), parse_num(y_str))
}
