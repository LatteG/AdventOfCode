use std::collections::HashMap;
use rayon::prelude::*;
use num::{integer::lcm, Integer};

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Clone, Debug, Copy)]
enum Direction {
    L,
    R,
}

#[derive(PartialEq, Eq, Clone, Debug)]
struct Network {
    pattern:Vec<Direction>,
    nodes:HashMap<String, (String, String)>,
}

impl Network {
    fn step(&self, curr:String, dir:Direction) -> String {
        let (left, right):(String, String) = self.nodes.get(&curr).unwrap().clone();
        match dir {
            Direction::L => left,
            Direction::R => right
        }
    }

    fn follow_network(&self, start:String, end:String) -> u64 {
        let mut next_dir:Direction = Direction::L;
        let mut next_node:String = start;
        let mut steps:u64 = 0;
        while next_node != end {
            next_dir = self.pattern.get(steps as usize % self.pattern.len()).unwrap().clone();
            next_node = self.step(next_node, next_dir);
            steps += 1;
        }
        return steps;
    }

    fn find_loop(&self, start_node:String) -> (u64, u64) {
        let mut visited_nodes:Vec<(String, u64)> = Vec::new();
        let mut steps:u64 = 0;
        let mut next_dir:Direction = self.pattern.get(0).unwrap().clone();
        let mut loop_not_found:bool = true;
        let mut next_node:String = start_node;

        while loop_not_found {
            visited_nodes.push((next_node.clone(), steps % self.pattern.len() as u64));
            steps += 1;

            next_node = self.step(visited_nodes.last().unwrap().0.clone(), next_dir.clone());
            next_dir = self.pattern.get(steps as usize % self.pattern.len()).unwrap().clone();
            loop_not_found = !visited_nodes.contains(&(next_node.clone(), steps % self.pattern.len() as u64));
        }

        let loop_start_index:u64 = visited_nodes.iter().position(|n:&(String, u64)| n == &(next_node.clone(), steps % self.pattern.len() as u64)).unwrap() as u64;
        (loop_start_index, visited_nodes.len() as u64 - loop_start_index)
    }

    fn parallell_follow_network(&self, start_postfix:char, end_postfix:char) -> u64 {
        let start_nodes:Vec<String> = self.nodes.keys().filter(|k| k.ends_with(start_postfix)).map(|s| s.clone()).collect();
        let (loop_offsets, loop_lengths):(Vec<u64>, Vec<u64>) = start_nodes.par_iter().map(|node| self.find_loop(node.clone())).unzip();
        println!("\nLoop offsets:\n{:?}\n\nLoop lengths:\n{:?}\n", loop_offsets, loop_lengths);
        loop_lengths.iter().fold(1, |acc, num| acc.lcm(num))
    }
}

pub fn task1(input:&str) {
    let network:Network = parse_network(input);
    let steps:u64 = network.follow_network("AAA".to_string(), "ZZZ".to_string());
    println!("From AAA to ZZZ in {} steps", steps);
}

pub fn task2(input:&str) {
    let network:Network = parse_network(input);
    let steps:u64 = network.parallell_follow_network('A', 'Z');
    println!("From all __A to all __Z in {} steps", steps);
}

fn parse_network(input:&str) -> Network {
    let (pattern_str, nodes_str):(&str, &str) = input.split_once("\n\n").unwrap();

    let pattern:Vec<Direction> = pattern_str.chars().map(|c| char_to_direction(c)).collect();
    let nodes:HashMap<String, (String, String)> =  nodes_str.split("\n").map(|s| str_to_node(s)).collect();

    Network {
        pattern:pattern,
        nodes:nodes,
    }
}

fn char_to_direction(c:char) -> Direction {
    match c {
        'L' => Direction::L,
        'R' => Direction::R,
        _   => panic!("Problem parsing direction!")
    }
}

fn str_to_node(node_str:&str) -> (String, (String, String)) {
    let (label, neighbours):(&str, &str) = node_str.split_once(" = ").unwrap();
    let (left, right):(&str, &str) = neighbours.trim_matches(&['(', ')']).split_once(", ").unwrap();

    (label.to_string(), (left.to_string(), right.to_string()))
}
