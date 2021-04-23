use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use utils::*;

#[derive(Clone, Debug)]
pub enum Constraint {
    NotEqual(usize, usize),
    GreaterThan(usize, usize), // .0 must be greater than .1
}

#[derive(Clone, Debug)]
pub enum PossibleValue {
    None,
    Value(u64),
    Ranges(Vec<std::ops::Range<u64>>),
}

impl PossibleValue {
    pub fn as_u64(&self) -> Option<u64> {
        match self {
            PossibleValue::Value(x) => Some(*x),
            PossibleValue::Ranges(_) => None,
            PossibleValue::None => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Var {
    value: PossibleValue,
}

impl Var {
    pub fn as_u64(&self) -> Option<u64> {
        self.value.as_u64()
    }

    pub fn as_u64_biggest(&self) -> Option<u64> {
        match &self.value {
            PossibleValue::None => None,
            PossibleValue::Value(v) => Some(*v),
            PossibleValue::Ranges(ranges) => {
                if ranges.len() == 0 {
                    None
                } else {
                    let mut biggest = u64::min_value();
                    for r in ranges {
                        biggest = biggest.max(r.end - 1);
                    }
                    Some(biggest)
                }
            }
        }
    }

    pub fn as_u64_smallest(&self) -> Option<u64> {
        match &self.value {
            PossibleValue::None => None,
            PossibleValue::Value(v) => Some(*v),
            PossibleValue::Ranges(ranges) => {
                if ranges.len() == 0 {
                    None
                } else {
                    let mut smallest = u64::max_value();
                    for r in ranges {
                        smallest = smallest.min(r.end - 1);
                    }
                    Some(smallest)
                }
            }
        }
    }
}

pub enum VarChooseError {
    OutOfBounds,
}

pub enum RemoveValueResult {
    Ok,
    None,
    Value,
}

impl Var {
    // pub fn choose_one(&mut self) -> Option<u64> {
    //     let v = match &self.value {
    //         PossibleValue::Ranges(ranges) => match ranges.iter().nth(0) {
    //             Some(a) => PossibleValue::Value(a.start),
    //             None => PossibleValue::None,
    //         },
    //         PossibleValue::Value(v) => PossibleValue::Value(*v),
    //         PossibleValue::None => PossibleValue::None,
    //     };
    //     self.value = v;
    //     self.value.as_u64()
    // }

    pub fn choose(&mut self, index: usize) -> std::result::Result<u64, VarChooseError> {
        match &self.value {
            PossibleValue::Ranges(ranges) => {
                let mut index = index as u64;
                for i in 0..ranges.len() {
                    let start = ranges[i].start;
                    let end = ranges[i].end;
                    let len = end - start;
                    if index <= len {
                        let v = start + index;
                        self.value = PossibleValue::Value(v);
                        return Ok(v);
                    } else {
                        index -= len;
                    }
                }
                Err(VarChooseError::OutOfBounds)
            }
            PossibleValue::Value(v) => {
                if index == 0 {
                    Ok(*v)
                } else {
                    Err(VarChooseError::OutOfBounds)
                }
            }
            PossibleValue::None => Err(VarChooseError::OutOfBounds),
        }
    }

    pub fn remove_value(&mut self, value: u64) -> RemoveValueResult {
        let mut res = RemoveValueResult::Ok;
        self.value = match &self.value {
            PossibleValue::None => PossibleValue::None,
            PossibleValue::Value(v) => {
                if *v != value {
                    PossibleValue::Value(*v)
                } else {
                    res = RemoveValueResult::None;
                    PossibleValue::None
                }
            }
            PossibleValue::Ranges(ranges) => {
                let mut newranges = Vec::new();
                for r in ranges {
                    if r.contains(&value) {
                        let a = r.start..value;
                        if a.start != a.end {
                            newranges.push(a);
                        }
                        let a = (value + 1)..r.end;
                        if a.start != a.end {
                            newranges.push(a);
                        }
                    } else {
                        newranges.push(r.clone())
                    }
                }
                if newranges.len() == 0 {
                    res = RemoveValueResult::None;
                    PossibleValue::None
                } else if newranges.len() == 1 {
                    let r = &newranges[0];
                    if r.end - r.start == 1 {
                        res = RemoveValueResult::Value;
                        PossibleValue::Value(r.start)
                    } else {
                        PossibleValue::Ranges(newranges)
                    }
                } else {
                    PossibleValue::Ranges(newranges)
                }
            }
        };

        res
    }

    pub fn remove_value_smaller_than(&mut self, value: u64) -> RemoveValueResult {
        let mut res = RemoveValueResult::Ok;
        self.value = match &self.value {
            PossibleValue::None => PossibleValue::None,
            PossibleValue::Value(v) => {
                if *v >= value {
                    PossibleValue::Value(*v)
                } else {
                    res = RemoveValueResult::None;
                    PossibleValue::None
                }
            }
            PossibleValue::Ranges(ranges) => {
                let mut newranges = Vec::new();
                for r in ranges {
                    let mut r = r.clone();
                    if r.start <= value {
                        r.start = value;
                    }

                    if r.end - r.start > 1 {
                        newranges.push(r);
                    }
                }

                if newranges.len() == 0 {
                    res = RemoveValueResult::None;
                    PossibleValue::None
                } else if newranges.len() == 1 {
                    let r = &newranges[0];
                    if r.end - r.start == 1 {
                        res = RemoveValueResult::Value;
                        PossibleValue::Value(r.start)
                    } else {
                        PossibleValue::Ranges(newranges)
                    }
                } else {
                    PossibleValue::Ranges(newranges)
                }
            }
        };

        res
    }

    pub fn remove_value_greater_than(&mut self, value: u64) -> RemoveValueResult {
        let mut res = RemoveValueResult::Ok;
        self.value = match &self.value {
            PossibleValue::None => PossibleValue::None,
            PossibleValue::Value(v) => {
                if *v <= value {
                    PossibleValue::Value(*v)
                } else {
                    res = RemoveValueResult::None;
                    PossibleValue::None
                }
            }
            PossibleValue::Ranges(ranges) => {
                let mut newranges = Vec::new();
                for r in ranges {
                    let mut r = r.clone();
                    if r.end <= value {
                        r.end = value;
                    }

                    if r.end - r.start > 1 {
                        newranges.push(r);
                    }
                }

                if newranges.len() == 0 {
                    res = RemoveValueResult::None;
                    PossibleValue::None
                } else if newranges.len() == 1 {
                    let r = &newranges[0];
                    if r.end - r.start == 1 {
                        res = RemoveValueResult::Value;
                        PossibleValue::Value(r.start)
                    } else {
                        PossibleValue::Ranges(newranges)
                    }
                } else {
                    PossibleValue::Ranges(newranges)
                }
            }
        };

        res
    }
}

#[derive(Clone, Debug)]
pub struct Model {
    vars: Vec<Var>,
    constraints: Arc<Mutex<Vec<Constraint>>>,
    cons_by_var: Arc<Mutex<HashMap<usize, Vec<usize>>>>,
    has_none: bool,
}

impl Model {
    pub fn new() -> Self {
        Self {
            vars: Vec::new(),
            constraints: Arc::new(Mutex::new(Vec::new())),
            cons_by_var: Arc::new(Mutex::new(HashMap::new())),
            has_none: false,
        }
    }

    pub fn push_var(&mut self, value: PossibleValue) {
        self.vars.push(Var { value });
    }

    pub fn get_var(&mut self, index: usize) -> Option<&Var> {
        self.vars.get(index)
    }

    pub fn push_constraint(&mut self, c: Constraint) {
        let mut cons_by_var = self.cons_by_var.lock().unwrap();
        let mut allconstraints = self.constraints.lock().unwrap();

        let cid = allconstraints.len();
        match &c {
            Constraint::NotEqual(l, r) => {
                let cs = cons_by_var.entry(*l).or_default();
                cs.push(cid);
                let cs = cons_by_var.entry(*r).or_default();
                cs.push(cid);
            }
            Constraint::GreaterThan(l, r) => {
                let cs = cons_by_var.entry(*l).or_default();
                cs.push(cid);
                let cs = cons_by_var.entry(*r).or_default();
                cs.push(cid);
            }
        }
        allconstraints.push(c)
    }

    fn is_ok(&self) -> bool {
        let allconstraints = self.constraints.lock().unwrap();
        for c in allconstraints.iter() {
            match c {
                Constraint::NotEqual(l, r) => {
                    let vl = self.vars[*l].as_u64();
                    let vr = self.vars[*r].as_u64();
                    let isok = match (vl, vr) {
                        (None, _) => false,
                        (_, None) => false,
                        (Some(l), Some(r)) => l != r,
                    };

                    if !isok {
                        return false;
                    }
                }
                Constraint::GreaterThan(l, r) => {
                    let vl = self.vars[*l].as_u64();
                    let vr = self.vars[*r].as_u64();
                    let isok = match (vl, vr) {
                        (None, _) => false,
                        (_, None) => false,
                        (Some(l), Some(r)) => l > r,
                    };

                    if !isok {
                        return false;
                    }
                }
            }
        }

        true
    }

    fn propagate_constraints(&mut self, index: usize) {
        let mut call = std::collections::BTreeSet::new();

        let var = self.vars.get_mut(index).unwrap();
        if let Some(value) = var.as_u64() {
            let cons_by_var = self.cons_by_var.lock().unwrap();
            let allconstraints = self.constraints.lock().unwrap();
            match cons_by_var.get(&index) {
                Some(constraints) => {
                    for cid in constraints {
                        if self.has_none {
                            return;
                        }
                        match allconstraints.get(*cid) {
                            Some(Constraint::NotEqual(l, r)) => {
                                if *l == index || *r == index {
                                    let index = if *l == index { *r } else { *l };
                                    let var = self.vars.get_mut(index).unwrap();

                                    match var.remove_value(value) {
                                        RemoveValueResult::None => {
                                            self.has_none = true;
                                            return;
                                        }
                                        RemoveValueResult::Value => {
                                            call.insert(index);
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            Some(Constraint::GreaterThan(l, r)) => {
                                let lvalue = self.vars.get_mut(*l).unwrap();
                                let lvalue = lvalue.as_u64_smallest();
                                let rvalue = self.vars.get_mut(*r).unwrap();
                                let rvalue = rvalue.as_u64_biggest();

                                match (lvalue, rvalue) {
                                    (Some(lvalue), Some(rvalue)) => {
                                        let l = self.vars.get_mut(*l).unwrap();
                                        l.remove_value_smaller_than(rvalue);
                                        let r = self.vars.get_mut(*r).unwrap();
                                        r.remove_value_greater_than(lvalue);
                                    }
                                    _ => {}
                                }
                            }
                            None => {}
                        }
                    }
                }
                None => {}
            }
        }

        for index in call {
            if self.has_none {
                return;
            }
            self.propagate_constraints(index);
        }
    }

    fn set_value_index(&mut self, index: usize, value_index: usize) -> Result<u64, VarChooseError> {
        let var = self.vars.get_mut(index).unwrap();
        match var.choose(value_index) {
            Ok(value) => {
                self.propagate_constraints(index);
                Ok(value)
            }
            Err(e) => Err(e),
        }
    }

    fn solve_internal(&self, vari: usize) -> Option<Model> {
        if vari >= self.vars.len() {
            return None;
        }

        let mut varv = 0;

        loop {
            // println!("{:?}", self.vars);

            let mut m2 = self.clone();
            match m2.set_value_index(vari, varv) {
                Ok(_) => {
                    if m2.has_none {
                    } else if m2.is_ok() {
                        return Some(m2);
                    } else if let Some(m3) = m2.solve_internal(vari + 1) {
                        return Some(m3);
                    }
                }
                Err(VarChooseError::OutOfBounds) => {
                    return None;
                }
            }

            varv += 1;
        }
    }

    fn map_sol(&self) -> Vec<u64> {
        self.vars.iter().map(|x| x.as_u64().unwrap()).collect()
    }

    pub fn solve(&mut self) -> Result<Vec<u64>, ()> {
        let m = self.clone();
        if let Some(m) = m.solve_internal(0) {
            Ok(m.map_sol())
        } else {
            Err(())
        }
    }

    // (source, dest)
    pub fn shuffle_vars(&mut self, newpos: Vec<(usize, usize)>) {
        // println!("{:?}", newpos);

        let mut vars: std::collections::HashMap<usize, Var> =
            self.vars.drain(..).enumerate().collect();

        for (s, _) in newpos.iter() {
            let v = vars.remove(&s).unwrap();
            self.vars.push(v);
        }

        let vars: std::collections::HashMap<usize, usize> =
            newpos.iter().map(|x| x.clone()).collect();

        let mut allconstraints = self.constraints.lock().unwrap();
        for c in allconstraints.iter_mut() {
            match c {
                Constraint::NotEqual(l, r) => {
                    *l = vars[l];
                    *r = vars[r];
                }

                Constraint::GreaterThan(l, r) => {
                    *l = vars[l];
                    *r = vars[r];
                }
            }
        }

        let mut cons_by_var = self.cons_by_var.lock().unwrap();
        cons_by_var.clear();
        for (cid, c) in allconstraints.iter().enumerate() {
            match &c {
                Constraint::NotEqual(l, r) => {
                    let cs = cons_by_var.entry(*l).or_default();
                    cs.push(cid);
                    let cs = cons_by_var.entry(*r).or_default();
                    cs.push(cid);
                }
                Constraint::GreaterThan(l, r) => {
                    let cs = cons_by_var.entry(*l).or_default();
                    cs.push(cid);
                    let cs = cons_by_var.entry(*r).or_default();
                    cs.push(cid);
                }
            }
        }
    }
}

fn main() {
    let file = std::env::args().nth(1).unwrap();
    let mut file = std::fs::OpenOptions::new().read(true).open(file).unwrap();

    let mut model = Model::new();

    let vertices: u64 = file.read_value().unwrap();

    let edges: u64 = file.read_value().unwrap();
    for _ in 0..edges {
        let v0: u64 = file.read_value().unwrap();
        let v1: u64 = file.read_value().unwrap();

        model.push_constraint(Constraint::NotEqual(v0 as usize, v1 as usize));
    }

    let max = 1000;
    let mut maxcolors = 2;
    let solution = loop {
        // println!("{:?}", maxcolors);
        if maxcolors >= max {
            break None;
        }
        let mut m = model.clone();

        for _ in 0..vertices {
            m.push_var(PossibleValue::Ranges(vec![0..maxcolors]));
        }
        for i in 1..vertices {
            model.push_constraint(Constraint::GreaterThan(0, i as usize));
        }

        let newpos = {
            let mut edges: std::collections::BTreeMap<usize, u64> =
                std::collections::BTreeMap::new();
            let allconstraints = m.constraints.lock().unwrap();
            for x in allconstraints.iter() {
                match x {
                    Constraint::NotEqual(l, r) => {
                        let v = edges.entry(*l).or_default();
                        *v += 1;
                        let v = edges.entry(*r).or_default();
                        *v += 1;
                    }
                    Constraint::GreaterThan(l, r) => {
                        let v = edges.entry(*l).or_default();
                        *v += 1;
                        let v = edges.entry(*r).or_default();
                        *v += 1;
                    }
                }
            }

            let mut edges: Vec<_> = edges.iter().collect();
            edges.sort_by_key(|x| x.1);
            edges.reverse();
            let newpos: Vec<_> = edges
                .iter()
                .enumerate()
                .map(|(dest, (source, _))| (**source, dest))
                .collect();
            newpos
        };
        m.shuffle_vars(newpos);

        if let Ok(solution) = m.solve() {
            break Some(solution);
        } else {
            maxcolors += 1;
        }
    };

    let solution = solution.unwrap();
    let mut colors = solution.iter();
    let colors = colors.distinct();

    println!("{} {}", colors.len(), 0);
    for c in solution {
        print!("{} ", c);
    }
    println!("");
}
