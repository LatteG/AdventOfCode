use std::collections::HashSet;

use petgraph::{prelude::GraphMap, visit::GetAdjacencyMatrix, Undirected};

type NetworkGraph<'a> = GraphMap<&'a str, i8, Undirected>;

pub fn task1(input:&str) {
    let network_graph: NetworkGraph = parse_input(input);
    let groups: HashSet<(String, String, String)> = get_3_groups(&network_graph);
    let potential_groups: HashSet<(String, String, String)> = groups.into_iter().filter(|tup| contains_chief(tup)).collect();
    println!("{} groups contain at least one computer starting with \"t\"", potential_groups.len());
}

pub fn task2(input:&str) {
    let network_graph: NetworkGraph = parse_input(input);
    let biggest_cluster: String = get_biggest_cluster(&network_graph);
    println!("Biggest cluster is: {}", biggest_cluster);
}

fn get_biggest_cluster(network_graph:&NetworkGraph) -> String {
    let mut biggest_cluster: Vec<&str> = Vec::new();
    let adjacency_matrix = network_graph.adjacency_matrix();

    for base_node in network_graph.nodes() {
        let mut neighbours: Vec<&str> = network_graph.neighbors(base_node).collect();
        loop {
            let mut missing_edge_count: Vec<u8> = vec![0; neighbours.len()];
            for index in 0..neighbours.len() {
                missing_edge_count[index] = neighbours.iter().fold(0, |acc, &neighbour| if network_graph.is_adjacent(&adjacency_matrix, neighbours[index], neighbour) || &neighbour == &neighbours[index] {acc} else {acc + 1});
            }

            if missing_edge_count.iter().all(|count| count == &0) {
                break
            }

            let mut max_index = 0;
            let mut max_val = missing_edge_count[0];
            for index in 1..neighbours.len() {
                if missing_edge_count[index] > max_val {
                    max_index = index;
                    max_val = missing_edge_count[index];
                }
            }

            neighbours.remove(max_index);
        }
        if neighbours.len() >= biggest_cluster.len() {
            biggest_cluster = neighbours;
            biggest_cluster.push(base_node);
        }
    }

    biggest_cluster.sort();
    biggest_cluster.join(",").to_string()
}

fn contains_chief((a, b, c):&(String, String, String)) -> bool {
    a.starts_with("t") || b.starts_with("t") || c.starts_with("t")
}

fn get_3_groups<'a>(network_graph:&NetworkGraph) -> HashSet<(String, String, String)> {
    let mut groups: HashSet<(String, String, String)> = HashSet::new();
    for node0 in network_graph.nodes() {
        let neighbours0: HashSet<&str> = network_graph.neighbors(node0).collect();
        for &node1 in &neighbours0 {
            let neighbours1: HashSet<&str> = network_graph.neighbors(node1).collect();
            for &node2 in neighbours0.intersection(&neighbours1) {
                let mut group = [node0, node1, node2];
                group.sort();
                groups.insert((group[0].to_string(), group[1].to_string(), group[2].to_string()));
            }
        }
    }
    groups
}

fn parse_input(input:&str) -> NetworkGraph {
    let mut network_graph: NetworkGraph = GraphMap::new();
    for connection in input.lines() {
        let (node_0, node_1): (&str, &str) = connection.split_once("-").unwrap();
        network_graph.add_edge(node_0, node_1, 1);
    }
    network_graph
}
