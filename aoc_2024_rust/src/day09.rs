use std::fmt::Debug;

#[derive(Copy, Clone)]
enum Memory {
    Free(usize),
    File(usize, i32),
}

impl Debug for Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Memory::Free(len) => write!(f, "Free: {}", len),
            Memory::File(len, id) => write!(f, "File {}: {}", id, len),
        }
    }
}

pub fn task1(input:&str) {
    let disk_map: Vec<i8> = parse_input(input);
    let uncompressed_disk: Vec<Memory> = get_uncompressed_disk(disk_map);
    let compressed_disk: Vec<Memory> = compress_disk(uncompressed_disk.clone());
    let checksum: i64 = calc_checksum(compressed_disk.clone());
    // print_disk(uncompressed_disk);
    // print_disk(compressed_disk);
    println!("Checksum is: {}", checksum);
}

pub fn task2(input:&str) {
    let disk_map: Vec<i8> = parse_input(input);
    let uncompressed_disk: Vec<Memory> = get_uncompressed_disk(disk_map);
    let compressed_disk: Vec<Memory> = compress_disk_no_fragmentation(uncompressed_disk.clone());
    let checksum: i64 = calc_checksum(compressed_disk.clone());
    // print_disk(uncompressed_disk);
    // print_disk(compressed_disk);
    println!("Checksum is: {}", checksum);
}

fn calc_checksum(compressed_disk:Vec<Memory>) -> i64 {
    let mut checksum: i64 = 0;
    let mut index: usize = 0;
    for file in compressed_disk {
        match file {
            Memory::Free(len) => index += len,
            Memory::File(len, id) => {
                // println!("  Index: {}\n  File: {:?}", index, file);
                for i in 0..len {
                    let delta_checksum: i32 = (index + i) as i32 * id;
                    // println!("    Delta checksum for index {}: {}", index + i, delta_checksum);
                    checksum += delta_checksum as i64;
                }
                // println!("  Checksum: {}", checksum);
                index += len;
            },
        }
    }
    checksum
}

fn print_disk(disk:Vec<Memory>) {
    let mut disk_strs: Vec<String> = Vec::new();
    for mem in disk {
        match mem {
            Memory::Free(len) => disk_strs.push(vec!["."; len].join("")),
            Memory::File(len, id) => disk_strs.push(vec![id.to_string(); len].join("")),
        }
    }
    println!("Disk:\n{}", disk_strs.join(""));
}

fn compress_disk_no_fragmentation(mut uncompressed_disk:Vec<Memory>) -> Vec<Memory> {
    let mut index: usize = 0;

    loop {
        if index >= uncompressed_disk.len() {
            loop {
                match uncompressed_disk.last().unwrap() {
                    Memory::Free(_) => {uncompressed_disk.pop();},
                    Memory::File(_, _) => return uncompressed_disk,
                }
            }
        }
        
        let mut last_fit: Memory = Memory::Free(0);
        let mut last_fit_size: usize = 0;
        let mut last_fit_index: usize = 0;

        match uncompressed_disk[index] {
            Memory::Free(free_len) => {
                let mut backward_index: usize = uncompressed_disk.len() - 1;
                while backward_index > index && last_fit_size == 0 {
                    match uncompressed_disk[backward_index] {
                        Memory::Free(_) => backward_index -= 1,
                        file @ Memory::File(file_len, _) if file_len <= free_len && file_len > last_fit_size => {
                            last_fit = file;
                            last_fit_size = file_len;
                            last_fit_index = backward_index;
                        },
                        Memory::File(_, _) => backward_index -= 1,
                    }
                }
                if last_fit_size > 0 {
                    uncompressed_disk = [uncompressed_disk[..last_fit_index].to_vec(), [Memory::Free(last_fit_size)].to_vec(), uncompressed_disk[last_fit_index + 1..].to_vec()].concat();
                    if last_fit_size == free_len {
                        uncompressed_disk = [uncompressed_disk[..index].to_vec(), [last_fit].to_vec(), uncompressed_disk[index + 1..].to_vec()].concat();
                    } else {
                        uncompressed_disk = [uncompressed_disk[..index].to_vec(), [last_fit, Memory::Free(free_len - last_fit_size)].to_vec(), uncompressed_disk[index + 1..].to_vec()].concat();
                    }
                }
                index += 1;
            },
            Memory::File(_, _) => index += 1,
        }
    }
}

fn compress_disk(mut uncompressed_disk:Vec<Memory>) -> Vec<Memory> {
    let mut index: usize = 0;

    loop {
        if index >= uncompressed_disk.len() {
            return uncompressed_disk;
        }
        
        match uncompressed_disk[index] {
            Memory::Free(free_len) => {
                match uncompressed_disk.last().unwrap().clone() {
                    Memory::Free(_) => {uncompressed_disk.pop();},
                    Memory::File(file_len, id) => {
                        if file_len == free_len {
                            uncompressed_disk[index] = uncompressed_disk.pop().unwrap();
                        } else if file_len > free_len {
                            uncompressed_disk[index] = Memory::File(free_len, id);
                            uncompressed_disk.pop();
                            uncompressed_disk.push(Memory::File(file_len - free_len, id));
                        } else {
                            uncompressed_disk = vec![uncompressed_disk[..index].to_vec(), vec![uncompressed_disk.pop().unwrap(), Memory::Free(free_len - file_len)], uncompressed_disk[index + 1..].to_vec()].concat();
                        }
                    },
                }
            },
            Memory::File(_, _) => index += 1,
        }
    }
}

fn get_uncompressed_disk(disk_map:Vec<i8>) -> Vec<Memory> {
    let mut file_id: i32 = 0;
    let mut uncompressed_disk: Vec<Memory> = Vec::new();

    for i in (0..disk_map.len()).step_by(2) {
        uncompressed_disk.push(Memory::File(disk_map[i] as usize, file_id));
        file_id += 1;
        if i + 1 < disk_map.len() {
            uncompressed_disk.push(Memory::Free(disk_map[i + 1] as usize));
        }
    }

    uncompressed_disk
}

fn parse_input(input:&str) -> Vec<i8> {
    input.chars().map(|c| c.to_string().parse::<i8>().unwrap()).collect()
}
