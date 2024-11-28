struct Race {
    time_ms: i64,
    dist_mm: i64,
}

impl ToString for Race {
    fn to_string(&self) -> String {
        format!("Race: {}ms, {}mm", self.time_ms, self.dist_mm)
    }
}

impl Race {
    fn get_dist_mm(&self, time_held_ms:i64) -> i64 {
        (self.time_ms - time_held_ms) * time_held_ms
    }
    fn beats_record(&self, time_held_ms:i64) -> bool {
        self.get_dist_mm(time_held_ms) > self.dist_mm
    }
    fn get_record_break_min_hold(&self) -> i64 {
        for time_held_ms in 1..(self.time_ms / 2) {
            if self.beats_record(time_held_ms) {
                return time_held_ms;
            }
        }
        return -1;
    }
    fn get_record_break_min_max_holds(&self) -> (i64, i64) {
        let min_time_held_ms:i64 = self.get_record_break_min_hold();
        let max_time_held_ms:i64 = self.time_ms - min_time_held_ms;
        (min_time_held_ms, max_time_held_ms)
    }
    fn get_race_score(&self) -> i64 {
        let (min_time, max_time):(i64, i64) = self.get_record_break_min_max_holds();
        max_time - min_time + 1
    }
}

pub fn task1(input:&str) {
    let races:Vec<Race> = parse_input(input);
    let races_scores:Vec<i64> = races.iter().map(|race| race.get_race_score()).collect();
    println!("Small races score: {}", races_scores.iter().fold(1, |acc, race| acc * race));
}

pub fn task2(input:&str) {
    let race:Race = parse_input_single_race(input);
    let race_score:i64 = race.get_race_score();
    println!("Big race score: {}", race_score);
}

fn parse_input_single_race(input:&str) -> Race {
    let (time_str, dist_str):(&str, &str) = input.split_once("\n").unwrap();
    let time:i64 = parse_line_bad_kerning(time_str);
    let dist:i64 = parse_line_bad_kerning(dist_str);
    Race{
        time_ms: time,
        dist_mm: dist,
    }
}

fn parse_line_bad_kerning(line:&str) -> i64 {
    match line.split_once(":").unwrap().1.split_whitespace().fold("".to_string(), |acc:String, num:&str| acc + num).parse::<i64>() {
        Ok(num) => num,
        Err(..) => panic!("Problem parsing number!"),
    }
}

fn parse_input(input:&str) -> Vec<Race> {
    let (time_strs, dist_strs):(&str, &str) = input.split_once("\n").unwrap();
    let times:Vec<i64> = parse_line(time_strs);
    let dists:Vec<i64> = parse_line(dist_strs);
    times.iter().zip(dists.iter()).map(|(time, dist):(&i64, &i64)| Race{time_ms: *time, dist_mm: *dist}).collect()
}

fn parse_line(line:&str) -> Vec<i64> {
    line.split_once(":").unwrap().1.trim().split_whitespace().map(|num_str| match num_str.parse::<i64>() {
        Ok(num) => num,
        Err(..) => panic!("Problem parsing number!"),
    }).collect()
}