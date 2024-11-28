enum RangeApply {
    Yes(i64),
    No,
}

enum OverlappingRange {
    FullyInside(Range),
    PartiallyInside(Range, Vec<Range>),
    No,
}

struct Range {
    min: i64,
    max: i64,
}

struct MappingRange {
    source: Range,
    dest_range_offset: i64,
}

struct Mapping {
    ranges: Vec<MappingRange>
}

impl Clone for Range {
    fn clone(&self) -> Range {
        Range{
            min: self.min,
            max: self.max,
        }
    }
}
impl Range {
    fn is_in_range(&self, val:i64) -> bool {
        val >= self.min && val <= self.max
    }
    fn split_range_at(&self, split_val:i64) -> (Range, Range) {
        (Range{min: self.min, max: split_val - 1}, Range{min: split_val, max: self.max})
    }
    fn overlapping_range(&self, range:Range) -> OverlappingRange {
        let seed_min_inside:bool = range.is_in_range(self.min);
        let seed_max_inside:bool = range.is_in_range(self.max);
        let range_min_inside:bool = self.is_in_range(range.min);
        let range_max_inside:bool = self.is_in_range(range.max);

        if seed_min_inside && seed_max_inside {
            OverlappingRange::FullyInside(self.clone())
        } else if seed_max_inside && range_min_inside {
            let (outside, inside):(Range, Range) = self.split_range_at(range.min);
            OverlappingRange::PartiallyInside(inside, vec![outside])
        } else if seed_min_inside && range_max_inside {
            let (inside, outside):(Range, Range) = self.split_range_at(range.max + 1);
            OverlappingRange::PartiallyInside(inside, vec![outside])
        } else if range_min_inside && range_max_inside {
            let (outside0, inside0):(Range, Range) = self.split_range_at(range.min);
            let (inside1, outside1):(Range, Range) = inside0.split_range_at(range.max + 1);
            OverlappingRange::PartiallyInside(inside1, vec![outside0, outside1])
        } else {
            OverlappingRange::No
        }
    }
}

impl MappingRange {
    fn apply_seed(&self, val:i64) -> RangeApply {
        if self.source.is_in_range(val) {
            RangeApply::Yes(val + self.dest_range_offset)
        } else {
            RangeApply::No
        }
    }
    fn apply_seed_range(&self, range:Range) -> OverlappingRange {
        match range.overlapping_range(self.source.clone()) {
            OverlappingRange::FullyInside(r) => OverlappingRange::FullyInside(Range{min: r.min + self.dest_range_offset, max: r.max + self.dest_range_offset}),
            OverlappingRange::PartiallyInside(r, v) => OverlappingRange::PartiallyInside(Range{min: r.min + self.dest_range_offset, max: r.max + self.dest_range_offset}, v),
            OverlappingRange::No => OverlappingRange::No,
        }
    }
}

impl Mapping {
    fn apply_mapping(&self, val:i64) -> i64 {
        let applied_val:RangeApply = self.ranges.iter().fold(RangeApply::No, |acc:RangeApply, range:&MappingRange| match acc {
            RangeApply::No => range.apply_seed(val),
            yes => yes,
        });
        match applied_val {
            RangeApply::Yes(v) => v,
            RangeApply::No => val,
        }
    }
    fn apply_mapping_to_range(&self, seed_range:Range) -> Vec<Range> {
        let overlapping_range:OverlappingRange = self.ranges.iter().fold(OverlappingRange::No, |acc:OverlappingRange, range:&MappingRange| match acc {
            OverlappingRange::No => range.apply_seed_range(seed_range.clone()),
            yes => yes,
        });
        match overlapping_range {
            OverlappingRange::No => vec![seed_range.clone()],
            OverlappingRange::FullyInside(new_seed_range) => vec![new_seed_range],
            OverlappingRange::PartiallyInside(new_seed_range, additional_seed_ranges) => {
                let matched_vec:Vec<Range> = vec![new_seed_range]; 
                let split_range_vecs:Vec<Range> = additional_seed_ranges.iter().map(|sr| self.apply_mapping_to_range(sr.clone())).collect::<Vec<Vec<Range>>>().concat();
                [matched_vec, split_range_vecs].concat()
            },
        }
    }
}

pub fn task1(input:&str) {
    let (seeds, mappings):(Vec<i64>, Vec<Mapping>) = parse_input(input);
    let locations:Vec<i64> = seeds.iter().map(|s:&i64| get_seed_location(*s, &mappings)).collect();
    println!("Single seed location: {}", locations.iter().min().unwrap());
}

pub fn task2(input:&str) {
    let (seeds, mappings):(Vec<i64>, Vec<Mapping>) = parse_input(input);
    let seed_ranges:Vec<Range> = get_ranges_from_seeds(seeds);
    let location_ranges:Vec<Range> = apply_seed_ranges_to_mappings(seed_ranges, &mappings);
    println!("Seed range location: {}", get_min_location_from_ranges(location_ranges));
}

fn get_min_location_from_ranges(location_ranges:Vec<Range>) -> i64 {
    location_ranges.iter().map(|lr| lr.min).min().unwrap()
}

fn apply_seed_ranges_to_mappings(seed_ranges:Vec<Range>, mappings:&Vec<Mapping>) -> Vec<Range> {
    mappings.iter().fold(seed_ranges, |acc:Vec<Range>, mapping:&Mapping| acc.iter().map(|seed_range:&Range| mapping.apply_mapping_to_range(seed_range.clone())).collect::<Vec<Vec<Range>>>().concat())
}

fn get_ranges_from_seeds(seed_ranges:Vec<i64>) -> Vec<Range> {
    let mut ranges:Vec<Range> = Vec::new();
    for i in (0..seed_ranges.len()).step_by(2) {
        ranges.push(Range{
            min: seed_ranges[i],
            max: seed_ranges[i] + seed_ranges[i + 1] - 1,
        });
    }
    return ranges;
}

fn get_seed_location(seed:i64, mappings:&Vec<Mapping>) -> i64 {
    mappings.iter().fold(seed, |val:i64, mapping:&Mapping| mapping.apply_mapping(val))
}

fn parse_input(input:&str) -> (Vec<i64>, Vec<Mapping>) {
    let (seeds_str, mappings_str):(&str, &str) = input.split_once("\n\n").unwrap();

    let seeds:Vec<i64> = seeds_str.split_once(" ").unwrap().1.split_whitespace().map(|n| parse_number(n.trim())).collect();

    let mapping_strs:Vec<&str> = mappings_str.split("\n\n").collect();
    let mappings:Vec<Mapping> = mapping_strs.iter().map(|m| parse_mapping(*m)).collect();

    return (seeds, mappings);
}

fn parse_number(number:&str) -> i64 {
    match number.parse::<i64>() {
        Ok(n) => n,
        Err(..) => panic!("Parse error for number!")
    }
}

fn parse_mapping(mapping_str:&str) -> Mapping {
    let range_strs:Vec<&str> = mapping_str.split_once(":").unwrap().1.trim().split("\n").collect();
    Mapping {
        ranges: range_strs.iter().map(|r| parse_mapping_range(*r)).collect(),
    }
}

fn parse_mapping_range(range_str:&str) -> MappingRange {
    let range_nums:Vec<i64> = range_str.split_whitespace().map(|n| parse_number(n)).collect();
    let dest_start:i64 = range_nums[0];
    let source_start:i64 = range_nums[1];
    let range_len:i64 = range_nums[2];
    MappingRange{
        source: Range {
            min: source_start,
            max: source_start + range_len - 1,
        },
        dest_range_offset: dest_start - source_start,
    }
}