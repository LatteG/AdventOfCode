use std::{collections::VecDeque, fmt::Debug};

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Clone, Copy)]
enum Condition {
    Operational,
    Damaged,
    Unknown
}

impl Debug for Condition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Operational => write!(f, "."),
            Self::Damaged => write!(f, "#"),
            Self::Unknown => write!(f, "?"),
        }
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Clone)]
struct Record {
    conditions:Vec<Condition>,
    breakages:Vec<u32>,
}

impl Debug for Record {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Record").field("conditions", &self.conditions).field("breakages", &self.breakages).finish()
    }
}

impl Record {
    fn get_arrengment_amount(&self) -> u32 {
        let all_conditions:Vec<Vec<Condition>> = self.get_all_conditions(vec![], self.conditions.clone());

        println!("Breakages: {:?}", self.breakages);
        println!("All conditions:");
        all_conditions.iter().map(|conds| println!("{:?}", conds));
        
        all_conditions.iter().fold(0, |acc, conditions| if self.matches_breakage(conditions) {
            acc + 1
        } else {
            acc
        })
    }

    fn get_all_conditions(&self, mut acc0:Vec<Condition>, mut remaining:Vec<Condition>) -> Vec<Vec<Condition>> {
        let mut val:Option<Condition> = remaining.pop();

        while val == Some(Condition::Damaged) || val == Some(Condition::Operational) {
            acc0.push(val.unwrap());
        }

        match val {
            None => {
                acc0.reverse();
                vec![acc0]
            },
            Some(Condition::Unknown) => {
                let mut acc1:Vec<Condition> = acc0.clone();
                acc0.push(Condition::Damaged);
                acc1.push(Condition::Operational);

                let mut damaged_half:Vec<Vec<Condition>> = self.get_all_conditions(acc0, remaining.clone());
                let mut operational_half:Vec<Vec<Condition>> = self.get_all_conditions(acc1, remaining);
                damaged_half.append(&mut operational_half);
                damaged_half
            },
            _ => panic!("Porblems constructing all conditions!"),
        }
    }

    fn matches_breakage(&self, conditions_candidate:&Vec<Condition>) -> bool {
        let mut found_breakages:Vec<u32> = Vec::new();
        let mut acc:u32 = 0;

        for cond in conditions_candidate {
            match *cond {
                Condition::Operational => {
                    if acc > 0 {
                        found_breakages.push(acc);
                        acc = 0;
                    }
                },
                Condition::Damaged     => acc += 1,
                Condition::Unknown     => panic!("Trying to match unprocessed conditions vector!"),
            }
        }
        if acc > 0 {
            found_breakages.push(acc);
        }

        found_breakages == self.breakages
    }
}

pub fn task1(input:&str) {
    let records:Vec<Record> = parse_input(input);
    //println!("Records:\n{:?}", records);
    let arrangements:Vec<u32> = records.iter().map(|record| record.get_arrengment_amount()).collect();
    println!("Total amount of arrengements: {}", arrangements.iter().sum::<u32>());
}

pub fn task2(input:&str) {
    todo!()
}

fn parse_input(input:&str) -> Vec<Record> {
    input.split("\n").map(|l| parse_line(l)).collect()
}

fn parse_line(line:&str) -> Record {
    let (condition_str, breakage_list):(&str, &str) = line.split_once(" ").unwrap();
    let conditions:Vec<Condition> = condition_str.chars().map(|c| char_to_condition(c)).collect();
    let breakages:Vec<u32> = breakage_list.split(",").map(|num_str| match num_str.parse::<u32>() {
        Ok(num) => num,
        Err(..) => panic!("Problem parsing breakage!"),
    }).collect();
    Record {
        conditions: conditions,
        breakages: breakages
    }
}

fn char_to_condition(c:char) -> Condition {
    match c {
        '.' => Condition::Operational,
        '#' => Condition::Damaged,
        '?' => Condition::Unknown,
        _   => panic!("Problems parsing condition!"),
    }
}