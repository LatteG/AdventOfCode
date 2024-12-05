struct PageOrderingRules {
    rules: Vec<(i32, i32)>,
}

impl PageOrderingRules {
    fn correctly_ordered_middle_page(&self, pages:&Vec<i32>) -> i32 {
        let mut correctly_ordered_pages: Vec<i32> = pages.clone();
        while !self.is_correctly_ordered(&correctly_ordered_pages) {
            correctly_ordered_pages = self.rules.iter().fold(pages.clone(), |acc, rule| self.update_order(acc, rule));
        }
        get_middle_page(correctly_ordered_pages)
    }

    fn update_order(&self, acc:Vec<i32>, rule:&(i32, i32)) -> Vec<i32> {
        match self.follows_rule(&acc, rule) {
            true => acc,
            false => self.swap_pages(acc, rule),
        }
    }

    fn swap_pages(&self, pages:Vec<i32>, (first, last):&(i32, i32)) -> Vec<i32> {
        let first_pos: usize = pages.iter().position(|p| p == first).unwrap();
        let last_pos: usize = pages.iter().position(|p| p == last).unwrap();

        let mut return_pages: Vec<i32> = pages.clone();
        return_pages[first_pos] = pages[last_pos];
        return_pages[last_pos] = pages[first_pos];

        return_pages
    }

    fn is_correctly_ordered(&self, pages:&Vec<i32>) -> bool {
        self.rules.iter().all(|rule| self.follows_rule(pages, rule))
    }

    fn follows_rule(&self, pages:&Vec<i32>, (first, last):&(i32, i32)) -> bool {
        let first_pos: Option<usize> = pages.iter().position(|p| p == first);
        let last_pos: Option<usize> = pages.iter().position(|p| p == last);
        match (first_pos, last_pos) {
            (Some(first_index), Some(last_index)) => first_index < last_index,
            (_, _) => true,
        }
    }
}

pub fn task1(input:&str) {
    let (rules, updates): (PageOrderingRules, Vec<Vec<i32>>) = parse_input(input);
    let allowed_updates: Vec<Vec<i32>> = updates.into_iter().filter(|pages| rules.is_correctly_ordered(pages)).collect();
    let allowed_middle_pages: Vec<i32> = allowed_updates.into_iter().map(|update_pages| get_middle_page(update_pages)).collect();

    println!("The sum of the allowed middle page updates is: {}", allowed_middle_pages.iter().sum::<i32>());
}

pub fn task2(input:&str) {
    let (rules, updates): (PageOrderingRules, Vec<Vec<i32>>) = parse_input(input);
    let incorrect_updates: Vec<Vec<i32>> = updates.into_iter().filter(|pages| !rules.is_correctly_ordered(pages)).collect();
    println!("Incorrect updates:\n{:?}", incorrect_updates);
    let updated_middle_pages: Vec<i32> = incorrect_updates.iter().map(|inc_upd| rules.correctly_ordered_middle_page(inc_upd)).collect();
    
    println!("Middle pages:\n{:?}", updated_middle_pages);
    println!("The sum of the updated incorrect middle page updates is: {}", updated_middle_pages.iter().sum::<i32>());
}

fn get_middle_page(pages:Vec<i32>) -> i32 {
    let middle_page_index: usize = pages.len() / 2;
    pages[middle_page_index]
}

fn parse_input(input:&str) -> (PageOrderingRules, Vec<Vec<i32>>) {
    let (ordering_str, update_str): (&str, &str) = input.split_once("\n\n").unwrap();
    
    let ordering_rules: PageOrderingRules = parse_page_ordering_rules(ordering_str);
    let page_updates: Vec<Vec<i32>> = parse_page_updates(update_str);

    (ordering_rules, page_updates)
}

fn parse_page_ordering_rules(ordering_str:&str) -> PageOrderingRules {
    let lines: Vec<&str> = ordering_str.split_whitespace().collect();
    let rules: Vec<(i32,i32)> = lines.iter().map(|&rule_str|parse_rule(rule_str)).collect();
    PageOrderingRules{
        rules: rules,
    }
}

fn parse_rule(rule_str:&str) -> (i32, i32) {
    let (first_str, last_str): (&str, &str) = rule_str.split_once("|").unwrap();
    (parse_num(first_str), parse_num(last_str))
}

fn parse_page_updates(update_str:&str) -> Vec<Vec<i32>> {
    let line_strs: Vec<&str> = update_str.split_whitespace().collect();
    let update_strs: Vec<Vec<i32>> = line_strs.iter().map(|update_str| update_str.split(",").map(parse_num).collect()).collect();
    update_strs
}

fn parse_num(num_str:&str) -> i32 {
    match num_str.parse::<i32>() {
        Ok(num) => num,
        Err(_) => panic!("Problem parsing number!"),
    }
}
