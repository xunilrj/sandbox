use std::vec::Vec;
use std::ops::{Index, IndexMut};
use std::clone::Clone;

#[derive(Clone)]
struct GI
{
    index: usize,
    generation: u32,
}

#[derive(Clone)]
struct GenerationItem<T>
{
    generation: u32,
    data: Option<T>,
}

struct GenerationVec<T>
{
    items: Vec<GenerationItem<T>>,
    next: usize,
    none: Option<T>,
}

impl<T: Clone> GenerationVec<T>
{
    fn new() -> GenerationVec<T>
    {
        return GenerationVec { 
            items: vec![GenerationItem {generation: 0, data: None}; 10],
            next: 0,
            none: None
        };
    }

    fn push(&mut self, item: T) -> GI
    {
        while self.items[self.next].data.is_some()
        {
            self.next += 1;
        }

        let i = &mut self.items[self.next];
        i.generation += 1;
        i.data = Some(item);

        return GI {
            generation: i.generation,
            index: self.next,
        };
    }

    fn get<'a>(&'a self, i: &GI) -> Option<&'a T>
    {
        let item = &self.items[i.index];
        if item.data.is_none() || (item.generation != i.generation) { return None; }
        else { return item.data.as_ref(); }
    }

    fn remove(&mut self, i: GI)
    {
        let item = &mut self.items[i.index];
        if item.generation == i.generation
        {
            item.generation += 1;
            item.data = None;
        }
    }

    fn set(&mut self, i: &GI, v: T) -> Option<GI>
    {
        let item = &mut self.items[i.index];
        if item.data.is_some() && (item.generation == i.generation)
        {
            item.data = Some(v);
            return Some(i.clone());
        }

        None
    }

    fn set_if_none(&mut self, i: usize, v: T) -> Option<GI>
    {
        let item = &mut self.items[i];
        if item.data.is_none()
        {
            item.generation += 1;
            item.data = Some(v);
            return Some(GI {
                generation: item.generation,
                index: i,
            });
        }

        None
    }
}

type EntityId = usize;

#[derive(Clone)]
struct PlayerPosition
{
    x: f32,
    y: f32,
    z: f32,
}

fn main() {
    /*                                                                      POINTER APPROACH */
    // {
    //     // Simulate some players position
    //     let mut vec = vec![PlayerPosition{x: 0.0, y: 0.0, z: 0.0}; 3];
        
    //     //Simulate the player you want to follow
    //     //you get a pointer to the player you want to follow
    //     //and follow him, just writing his position
    //     let follow: EntityId = 0;
    //     let value = &vec[follow];
    //     println!("value: {}, {}, {}", value.x, value.y, value.z);

    //     //Simulate that for some reason the player is removed
    //     //he dies, he leaves the room, whatever...
    //     vec.remove(0);

    //     //Then later you try to follow him
    //     //again
    //     println!("value: {}, {}, {}", value.x, value.y, value.z);
    // }

    /*                                                                      INDEX APPROACH */
    // Simulate some players position
    let mut vec = vec![PlayerPosition{x: 0.0, y: 0.0, z: 0.0}; 3];
    vec[1].x = 1.0;
    vec[2].x = 2.0;
    
    //Simulate the player you want to follow
    //I keep JUST the player index in the array
    //never a pointer to the player position
    let follow: EntityId = 0;

    // I follow him, just writing his position
    // and release its "borrow" as soon as possible.
    {
        let value = &vec[follow];
        println!("value: {}, {}, {}", value.x, value.y, value.z);
    }

    //Simulate that for some reason the player is removed
    //he dies, he leaves the room, whatever...
    vec.remove(0);

    //Then later I try to follow him
    //again with... and it works!
    {
        let value = &vec[follow];
        println!("value: {}, {}, {}", value.x, value.y, value.z);
    }

    println!("Generational Indices");

    let mut items = GenerationVec::<PlayerPosition>::new();
    let i1 = items.push(PlayerPosition{x: 0.0, y: 0.0, z: 0.0});
    items.push(PlayerPosition{x: 0.0, y: 0.0, z: 0.0});
    items.push(PlayerPosition{x: 0.0, y: 0.0, z: 0.0});

    {
        let p1 = items.get(&i1);
        match p1 {
            Some(v) => println!("value: {} {} {}", v.x, v.y, v.z),
            None => println!("None")
        }
    }
    
    items.remove(i1.clone());

    {
        let p1 = items.get(&i1);
        match p1 {
            Some(v) => println!("value: {} {} {}", v.x, v.y, v.z),
            None => println!("None")
        }
    }

    let setr1 = items.set(&i1, PlayerPosition{x: 0.0, y: 10.0, z: 0.0});
    println!("Set Result None: {}", setr1.is_none());

    let setr2 = items.set_if_none(0, PlayerPosition{x: 0.0, y: 10.0, z: 0.0});
    println!("Set Result None: {}", setr2.is_none());
    
    match setr2
    {
        Some(i1_new) => {
            let p1 = items.get(&i1);
            match p1 {
                Some(v) => println!("value: {} {} {}", v.x, v.y, v.z),
                None => println!("None")
            }

            let p2 = items.get(&i1_new);
            match p2 {
                Some(v) => println!("value: {} {} {}", v.x, v.y, v.z),
                None => println!("None")
            }
        },
        None => println!("None")
    }

    // items[i1.clone()] = Some(1);

    // match items[i1.clone()] {
    //     Some(v) => println!("value: {}", v),
    //     None => println!("None")
    // }
}
