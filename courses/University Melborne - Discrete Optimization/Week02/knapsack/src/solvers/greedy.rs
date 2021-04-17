pub fn solve_internal(maxcapacity: u64, items: &[(usize, &crate::Item)]) -> (bool, u64, Vec<u8>) {
    let mut selected = vec![0; items.len()];

    let mut c = maxcapacity;
    let mut value = 0;
    for item in items {
        if c > item.1.weight {
            c -= item.1.weight;
            value += item.1.value;
            selected[item.0] = 1;
        }
    }

    (false, value, selected)
}

fn choose_best(av: &mut Option<u64>, a: &mut Vec<u8>, r: (bool, u64, Vec<u8>)) {
    if av.is_none() {
        *av = Some(r.1);
        *a = r.2;
    } else {
        let avv = av.unwrap();
        if avv < r.1 {
            *av = Some(r.1);
            *a = r.2;
        }
    }
}

pub fn solve(maxcapacity: u64, items: &[crate::Item]) -> (bool, u64, Vec<u8>) {
    let mut bv = None;
    let mut bs = Vec::new();

    let mut items: Vec<_> = items.iter().enumerate().collect();

    items.sort_by_key(|x| ((x.1.value as f32 / x.1.weight as f32) * 100.0) as u64);
    choose_best(
        &mut bv,
        &mut bs,
        solve_internal(maxcapacity, items.as_slice()),
    );
    items.reverse();
    choose_best(
        &mut bv,
        &mut bs,
        solve_internal(maxcapacity, items.as_slice()),
    );

    items.sort_by_key(|x| x.1.value);
    choose_best(
        &mut bv,
        &mut bs,
        solve_internal(maxcapacity, items.as_slice()),
    );
    items.reverse();
    choose_best(
        &mut bv,
        &mut bs,
        solve_internal(maxcapacity, items.as_slice()),
    );

    items.sort_by_key(|x| x.1.weight);
    choose_best(
        &mut bv,
        &mut bs,
        solve_internal(maxcapacity, items.as_slice()),
    );
    items.reverse();
    choose_best(
        &mut bv,
        &mut bs,
        solve_internal(maxcapacity, items.as_slice()),
    );

    println!("{:?}", bv);

    (false, 0, Vec::new())
}
