use crate::utils::*;

fn expand_solution(maxcapacity: u64, items: &[crate::Item], values: &Matrix<u64>) -> Vec<u8> {
    let mut selected = vec![0; items.len()];

    let mut c = maxcapacity as isize;
    for itemidx in (1..=items.len_isize()).rev() {
        let v = values.at(itemidx, c).unwrap();

        let previouscolumn = itemidx - 1;
        let a = values.at(previouscolumn, c).unwrap();

        if v != a {
            // println!("Using {} {}: {} vs {}", itemidx, c, v, a);
            let itemidx = itemidx as usize;
            selected[itemidx - 1] = 1;
            c -= (items[itemidx - 1].weight) as isize;
        } else {
            // println!("Not Using {} {}: {} vs {}", itemidx, c, v, a);
        }
    }

    selected
}

pub fn solve(maxcapacity: u64, items: &[crate::Item]) -> (bool, u64, Vec<u8>) {
    let maxcapacityisize = maxcapacity as isize;

    let cols = items.len() + 1;
    let rows = (maxcapacity + 1) as usize;
    let mut values: Matrix<u64> = Matrix::new(cols, rows);

    for itemidx in 1..=items.len_isize() {
        let item = items.get_isize(itemidx - 1).unwrap();
        let itemweight = item.weight as isize;
        let itemvalue = item.value;

        let previouscolumn = itemidx - 1;
        for capacity in 1..=maxcapacityisize {
            let a = values.at(previouscolumn, capacity);
            let b = values.at(previouscolumn, capacity - itemweight);
            let v = match (a, b) {
                (None, None) => 0,
                (None, Some(_)) => panic!("Should not be possible!"),
                (Some(&a), None) => a,
                (Some(&a), Some(&b)) => a.max(b + itemvalue),
            };
            values.set(itemidx, capacity, v);
        }
    }

    // println!("{:?}", values);
    let value = values
        .at(items.len() as isize, maxcapacity as isize)
        .unwrap();
    let items = expand_solution(maxcapacity, items, &values);

    (true, *value, items)
}
