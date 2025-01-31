use std::collections::HashMap;

use regex::Regex;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Gate {
    AND,
    OR,
    XOR
}

pub fn task1(input:&str) {
    let (vars, gates): (HashMap<String, bool>, Vec<(String, String, Gate, String)>) = parse_input(input);
    let new_vars: HashMap<String, bool> = calc_vars(vars.clone(), gates.clone());
    let output_num: u64 = get_var_value(&new_vars, "z");
    println!("Output: {}", output_num);
}

pub fn task2(input:&str) {
    let (vars, gates): (HashMap<String, bool>, Vec<(String, String, Gate, String)>) = parse_input(input);
    let x_val: u64 = get_var_value(&vars, "x");
    let y_val: u64 = get_var_value(&vars, "y");
    let z_goal_val: u64 = x_val + y_val;
    let mut swapped_wires: Vec<String> = Vec::new();
    let mut found_combo: bool = false;
    for s0 in 0..(gates.len() - 3) {
        for s1 in (s0 + 1)..(gates.len() - 2) {
            for s2 in (s1 + 1)..(gates.len() - 1) {
                for s3 in (s2 + 1)..(gates.len()) {
                    let (swapped_gates, curr_swapped_wires): (Vec<(String, String, Gate, String)>, Vec<String>) = swap_wires(gates.clone(), vec![s0, s1, s2, s3]);
                    let swapped_vars: HashMap<String, bool> = calc_vars(vars.clone(), swapped_gates.clone());
                    let z_val: u64 = get_var_value(&swapped_vars, "z");
                    if z_val == z_goal_val {
                        found_combo = true;
                        swapped_wires = curr_swapped_wires;
                        break;
                    }
                }
                if found_combo {
                    break;
                }
            }
            if found_combo {
                break;
            }
        }
        if found_combo {
            break;
        }
    }
    println!("Swapped wires:\n{}", swapped_wires.join(","));
}

fn swap_wires(mut gates:Vec<(String, String, Gate, String)>, indices:Vec<usize>) -> (Vec<(String, String, Gate, String)>, Vec<String>) {
    let mut swapped_wires: Vec<String> = Vec::new();
    for i in indices {
        let (in_0, in_1, g, out): (String, String, Gate, String) = gates[i].clone();
        gates[i] = (in_1.clone(), in_0.clone(), g, out);
        swapped_wires.push(in_0);
        swapped_wires.push(in_1);
    }
    swapped_wires.sort();
    (gates, swapped_wires)
}

fn calc_vars(mut vars:HashMap<String, bool>, mut gates:Vec<(String, String, Gate, String)>) -> HashMap<String, bool> {
    while gates.len() > 0 {
        let mut gates_to_remove: Vec<(String, String, Gate, String)> = Vec::new();
        for (in_0, in_1, op, out) in gates.clone() {
            if vars.contains_key(&in_0) && vars.contains_key(&in_1) {
                vars.insert(out.clone(), exec_gate(*vars.get(&in_0).unwrap(), *vars.get(&in_1).unwrap(), op));
                gates_to_remove.push((in_0, in_1, op, out));
            }
        }
        gates = gates.into_iter().filter(|g| !gates_to_remove.contains(g)).collect();
    }
    vars
}

fn get_var_value(vars:&HashMap<String, bool>, var_name:&str) -> u64 {
    let output_keys: Vec<&String> = vars.keys().filter(|&k| k.starts_with("z")).collect();
    let mut output_num: u64 = 0;
    for num in (0..output_keys.len()).rev() {
        let key: String = format!("{var_name}{num:02}").to_string();
        output_num *= 2;
        output_num += if *vars.get(&key).unwrap() {1} else {0};
    }
    output_num
}

fn exec_gate(b0:bool, b1:bool, gate:Gate) -> bool {
    match gate {
        Gate::AND => b0 && b1,
        Gate::OR  => b0 || b1,
        Gate::XOR => b0 ^ b1,
    }
}

fn parse_input(input:&str) -> (HashMap<String, bool>, Vec<(String, String, Gate, String)>) {
    let (start_vals_str, gates_str): (&str, &str) = input.split_once("\n\n").unwrap();

    let mut vars_map: HashMap<String, bool> = HashMap::new();
    for start_val_str in start_vals_str.lines() {
        let (val_name, bool_str) = start_val_str.split_once(": ").unwrap();
        let start_val: bool = bool_str == "1";
        vars_map.insert(val_name.to_string(), start_val);
    }

    let mut gates_list: Vec<(String, String, Gate, String)> = Vec::new();
    let re: Regex = Regex::new(r"(?<in_0>\w+)\s+(?<gate>\w+)\s+(?<in_1>\w+)\s+->\s+(?<out>\w+)").unwrap();
    for gate_str in gates_str.lines() {
        let captures: regex::Captures<'_> = re.captures(gate_str).unwrap();
        let gate: Gate = match &captures["gate"] {
            "AND" => Gate::AND,
            "OR"  => Gate::OR,
            "XOR" => Gate::XOR,
            _ => panic!("Unexpected gate found!")
        };
        gates_list.push((captures["in_0"].to_string(), captures["in_1"].to_string(), gate, captures["out"].to_string()));
    }

    (vars_map, gates_list)
}
