use regex::Regex;

pub fn task1(input:&str) {
    let numbers:Vec<Vec<u32>> = extract_numbers(input);
    let numbers2:Vec<u32> = numbers.iter().map(|l| l.first().unwrap() * 10 + l.last().unwrap()).collect();
    let sum:u32 = numbers2.iter().sum();
    println!("{}", sum);
}

pub fn task2(input:&str) {
    let numbers:Vec<Vec<u32>> = extract_numbers(translate_written_numbers(input).as_str());
    let numbers2:Vec<u32> = numbers.iter().map(|l| l.first().unwrap() * 10 + l.last().unwrap()).collect();
    let sum:u32 = numbers2.iter().sum();
    println!("{}", sum);
}

fn extract_numbers(input:&str) -> Vec<Vec<u32>> {
    input.trim().split_whitespace().map(|line| extract_numbers_from_line(line)).collect()
}

fn extract_numbers_from_line(line:&str) -> Vec<u32> {
    let re:Regex = Regex::new(r"[A-Za-z]").unwrap();
    let digits:String = re.replace_all(line.trim(), "").to_string();
    let numbers:Vec<u32> = digits.chars().map(|c| c.to_digit(10).unwrap()).collect();
    return numbers;
}

fn translate_written_numbers(input:&str) -> String {
    let return_val:String = input.to_lowercase()
                                 .replace("zero", "0zero")
                                 .replace("one", "o1ne")
                                 .replace("two", "t2wo")
                                 .replace("three", "th3ree")
                                 .replace("four", "fo4ur")
                                 .replace("five", "fi5ve")
                                 .replace("six", "si6x")
                                 .replace("seven", "sev7en")
                                 .replace("eight", "eig8ht")
                                 .replace("nine", "ni9ne");
    return return_val;
}