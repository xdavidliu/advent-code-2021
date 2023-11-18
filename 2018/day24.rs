use std::collections::HashSet;
use std::fs::read_to_string;
use Element::*;
use Team::*;

fn main() {
    let text = read_to_string("/home/xdavidliu/Documents/temp/data.txt").unwrap();
    let mut lines = text.lines();
    assert!(lines.next().unwrap().starts_with("Immune"));
    let mut groups = Vec::new();
    while let Some(line) = lines.next() {
        if line.is_empty() {
            break;
        } // the empty line between teams
        groups.push(Group::from(line, IMMUNE));
    }
    assert!(lines.next().unwrap().starts_with("Infection"));
    while let Some(line) = lines.next() {
        groups.push(Group::from(line, INFECT));
    }
    // use comments because too lazy to impl Clone for Group, to use groups twice
    // either that or parse twice

    // note anything between 16 and 34 inclusive would loop without finishing (I have
    // assertion that crashes), but that's WAI. 35 indeed is lowest boost that FINISHES with
    // immune win.
    // manually run binary search; can easily impl auto version
    println!("part 2 = {}", run(groups, 35)); // 13005
    // println!("part 1 = {}", run(groups, 0));  // 20150
}

fn run(mut groups: Vec<Group>, boost: i64) -> i64 {
    let mut attackers: Vec<_> = (0..groups.len()).collect();
    while both_teams_alive(&groups, &attackers) {
        attackers.sort_by_key(|&i| {
            let g = &groups[i];
            // decreasing order of effective power; in a tie ... higher initiative chooses first
            (-g.power(boost), -g.initiative)
        });
        let mut non_defenders: HashSet<_> = attackers.iter().copied().collect();
        let mut defenders = Vec::new();
        for &i in &attackers {
            let at = &groups[i];
            let t = non_defenders
                .iter()
                .filter(|&&m| groups[m].team != at.team)
                .max_by_key(|&&k| {
                    let def = &groups[k];
                    (
                        at.compute_damage(def, boost),
                        def.power(boost),
                        def.initiative,
                    )
                });
            // even though both teams alive; there may only be a few potential
            // defenders for one team
            if let Some(&def) = t {
                // ugly to calculate again, but cheap
                if 0 < at.compute_damage(&groups[def], boost) {
                    // the defender should have a pointer to attacker, since defender
                    // is the one that needs to mutate...
                    assert!(groups[def].attacker.is_none());
                    groups[def].attacker = Some(i);
                    defenders.push(def);
                    non_defenders.remove(&def);
                }
            }
        }
        // well I don't clear .attacker from previous runs, but that should be fine
        // because I only use defenders here. But I can clear anyways and see what happens
        assert!(!defenders.is_empty()); // TODO why fails?

        // sort the defenders by the initiative of their attackers
        defenders.sort_by_key(|&i| -groups[groups[i].attacker.unwrap()].initiative);
        for i in defenders {
            let defender = &groups[i];
            let attacker = &groups[defender.attacker.unwrap()];
            if attacker.units <= 0 {
                groups[i].attacker = None;
                continue;
            } // died
            let computed_damage = attacker.compute_damage(defender, boost);
            let defender_hp = defender.hp;
            groups[i].units -= computed_damage / defender_hp;
            groups[i].attacker = None;
        }
        attackers.retain(|&i| groups[i].units > 0);
    }
    let win = match groups[attackers[0].clone()].team {
        IMMUNE => "immune",
        INFECT => "infect",
    };
    println!("{win} won");
    attackers.iter().map(|a| groups[*a].units).sum()
}

fn both_teams_alive(groups: &[Group], attackers: &[usize]) -> bool {
    let first_attacker_team = &groups[attackers[0]].team;
    attackers
        .iter()
        .any(|&i| groups[i].team != *first_attacker_team)
}

struct Group {
    units: i64,
    hp: i64,
    immune: Vec<Element>,
    weak: Vec<Element>,
    damage: i64,
    element: Element,
    initiative: i64,
    team: Team,
    attacker: Option<usize>,
}

impl Group {
    fn from(line: &str, team: Team) -> Self {
        // 17 units each with 5390 hit points
        let mut first_part = line[0..line.find(" hit").unwrap()].split(' ');
        let units = first_part.next().unwrap().parse().unwrap();
        // units each with
        for _ in 0..3 {
            first_part.next();
        }
        let hp = first_part.next().unwrap().parse().unwrap();
        let last_index = line.find("that does ").unwrap() + "that does ".len();
        let mut third_part = line[last_index..].split(' ');
        let damage = third_part.next().unwrap().parse().unwrap();
        let element = Element::from(third_part.next().unwrap());
        // damage at initiative
        for _ in 0..3 {
            third_part.next();
        }
        let initiative = third_part.next().unwrap().parse().unwrap();
        let mut immune = Vec::new();
        let mut weak = Vec::new();
        let left_paren = line.find('(');
        if let Some(left_paren) = left_paren {
            let right_paren = line.find(')').unwrap();
            for section in line[left_paren + 1..right_paren].split("; ") {
                let destination = if section.starts_with("immune") {
                    &mut immune
                } else {
                    section.starts_with("weak");
                    &mut weak
                };
                let to_pos = section.find("to").unwrap();
                for word in section[to_pos + 3..].split(", ") {
                    destination.push(Element::from(word));
                }
            }
        }
        Self {
            units,
            hp,
            immune,
            weak,
            damage,
            element,
            initiative,
            team,
            attacker: None,
        }
    }
    fn actual_damage(&self, boost: i64) -> i64 {
        match self.team {
            IMMUNE => self.damage + boost,
            _ => self.damage,
        }
    }
    fn power(&self, boost: i64) -> i64 {
        self.units * self.actual_damage(boost)
    }
    fn compute_damage(&self, other: &Self, boost: i64) -> i64 {
        if other.immune.contains(&self.element) {
            0
        } else {
            let factor = if other.weak.contains(&self.element) {
                2
            } else {
                1
            };
            factor * self.units * self.actual_damage(boost)
        }
    }
}

#[derive(PartialEq)]
enum Element {
    SLASH,
    COLD,
    RADIATION,
    FIRE,
    BLUDGEON,
}

impl Element {
    fn from(word: &str) -> Self {
        match word {
            "fire" => FIRE,
            "slashing" => SLASH,
            "bludgeoning" => BLUDGEON,
            "cold" => COLD,
            "radiation" => RADIATION,
            _ => panic!(),
        }
    }
}

#[derive(PartialEq)]
enum Team {
    IMMUNE,
    INFECT,
}
