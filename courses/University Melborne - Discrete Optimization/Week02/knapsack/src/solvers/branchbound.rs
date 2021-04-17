fn calc_bound(_maxcapacity: u64, items: &[crate::Item], selected: &Vec<u8>) -> u64 {
    let mut value = 0;

    for i in 0..selected.len() {
        value += if selected[i] == 1 { items[i].value } else { 0 };
    }

    for i in selected.len()..items.len() {
        value += items[i].value;
    }

    value
}

fn calc_value(maxcapacity: u64, items: &[crate::Item], selected: &Vec<u8>) -> (u64, bool) {
    let mut value = 0;
    let mut weight = 0;
    for i in 0..items.len() {
        value += if selected[i] == 1 { items[i].value } else { 0 };
        weight += if selected[i] == 1 { items[i].weight } else { 0 };
    }

    (value, weight <= maxcapacity)
}

pub fn solve(maxcapacity: u64, items: &[crate::Item]) -> (bool, u64, Vec<u8>) {
    let selected = vec![];

    let mut q = Vec::new();
    q.push(selected.clone());

    let mut best = None;
    let mut bestv = None;

    loop {
        match q.pop() {
            Some(selected) => {
                if selected.len() == items.len() {
                    let (v, ok) = calc_value(maxcapacity, items, &selected);
                    if ok {
                        // println!("{:?} {:?}", selected, v);

                        let currentbest = bestv.unwrap_or(0);
                        if v > currentbest {
                            best = Some(selected.clone());
                            bestv = Some(v);
                        }
                    }
                    continue;
                } else {
                    let bound = calc_bound(maxcapacity, items, &selected);

                    // println!("Inter {:?} {} {:?}", selected, bound, bestv);

                    match bestv {
                        Some(bestv) => {
                            if bound < bestv {
                                // println!("prunning...");
                                continue;
                            }
                        }
                        None => {}
                    }

                    let a = [selected.as_slice(), &[0]].concat();
                    let b = [selected.as_slice(), &[1]].concat();

                    q.push(a);
                    q.push(b);
                }
            }
            None => break,
        }
    }

    // println!("{:?} {:?}", best, bestv);
    (true, bestv.unwrap(), best.unwrap())
}
