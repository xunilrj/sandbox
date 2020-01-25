# Generational Index

# Problem

We want to have a list of "components", here simplified as u32, but for example a position of a player. We want to allow another NPC to follow the player. So our NPC struct needs to "point" (the-unpronounceable-word) to the player is going to follow.

The first and immediate solution is to have a pointer to the players position and do whatever you want with it.

```rust
type EntityId = usize;

#[derive(Clone)]
struct PlayerPosition
{
    x: f32,
    y: f32,
    z: f32,
}

fn main() {
    // Simulate some players position
    let mut vec = vec![PlayerPosition{x: 0.0, y: 0.0, z: 0.0}; 3];
    
    //Simulate the player you want to follow
    //you get a pointer to the player you want to follow
    //and follow him, just writing his position
    let follow: EntityId = 0;
    let value = &vec[follow];
    println!("value: {}, {}, {}", value.x, value.y, value.z);

    //Simulate that for some reason the player is removed
    //he dies, he leaves the room, whatever...
    vec.remove(0);

    //Then later you try to follow him
    //again
    println!("value: {}, {}, {}", value.x, value.y, value.z);
}
```

The problem with this approach is that we will end up fighting the borrow checker and any attempt to keep this strategy will end up with the infamous "Vec<Rc<RefCell<PlayerPosition>>>". 

```
error[E0502]: cannot borrow `vec` as mutable because it is also borrowed as immutable
   --> src\main.rs:107:5
    |
104 |     let value = &vec[follow];
    |                  --- immutable borrow occurs here
...
107 |     vec.remove(0);
    |     ^^^^^^^^^^^^^ mutable borrow occurs here
108 |     println!("value: {}, {}, {}", value.x, value.y, value.z);
    |                                   ------- immutable borrow later used here
```

Another strategy is to deceive the borrow checker and instead of "borrowing a pointer" you "borrow the index of the pointer". As we say in Computer Science, "every borrow checker can be deceive by one layer of indirection". Hum... or something like this...

This strategy allow me to convince the borrow checker that everything is fine.

```rust
fn main() {
    // Simulate some players position
    let mut vec = vec![PlayerPosition{x: 0.0, y: 0.0, z: 0.0}; 3];
    
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
    //again
    {
        let value = &vec[follow];
        println!("value: {}, {}, {}", value.x, value.y, value.z);
    }
}
```

 It compiles... but it has a big bug... to understand let us first change the code a little bit:

 ```rust
fn main() {
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
}
```

If we run now, we will see:

```
value: 0, 0, 0
value: 1, 0, 0
```

Which means that the second "print" is actually printing the second item in the Vec. We exchanged a panic of a invalid pointer to a invisible bug. We are now following the wrong player. In some sense this is better, but it is also harder to diagnosis.

The problem here is somewhat similar to the famous ABA problem. Not the Swedish pop band, that is another problem but the concurrency problem.

ABA problem  
https://en.wikipedia.org/wiki/ABA_problem  

In one of Bjarne Stroustrup paper about ABA, there is a section about ABA avoidance techniques (2.1 Known ABA Avoidance Techniques I) where he cites that a well known strategy is to use "t is to apply a version tag attached to each value. The usage of version tags is the most commonly cited solution for ABA avoidance. The approach is effective, when it is possible to apply". He continues with a problem in the specific case of concurrency, which does not affect us here.

So we will try to tag each item in the Vec with a "version".

Understanding and Effectively Preventing the ABA Problem in Descriptor-based Lock-free Designs  
http://www.stroustrup.com/isorc2010.pdf  

The first thing we will do, is to create our version of Vec, that will enrich every item with a "version", here called generation.

```rust
#[derive(Clone)]
struct GenerationItem<T>
{
    generation: u32,
    data: Option<T>,
}

struct GenerationVec<T>
{
    items: Vec<GenerationItem<T>>,
}
```

We will preallocate every item that we need. We can improve on this later if needed.

```rust
impl<T: Clone> GenerationVec<T>
{
    fn new() -> GenerationVec<T>
    {
        return GenerationVec { 
            items: vec![GenerationItem {generation: 0, data: None}; 10],
        };
    }
}
```

Now we need to allow the insertion of items. But the idea of this strategy is to use the version to help me realize that the item I want was already deleted. So I will return both the index and the generation of the item.

```rust
impl<T: Clone> GenerationVec<T>
{
    ...
    fn push(&mut self, item: T)
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
        }
    }
    ...
}
```

Now I want to store this index and used it to retrieve some data.

```rust
fn main() {
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
}
```

Now I want to remove this item, try to get again and receive None instead of some random value. But first, let us implement the remove. We will implement the "remove" first borrowing the index, and later moving it.


```rust
impl<T: Clone> GenerationVec<T>
{
    ...
    fn remove(&mut self, i: &GI)
    {
        let item = &mut self.items[i.index];
        if item.generation == i.generation
        {
            item.generation += 1;
            item.data = None;
        }
    }
}
```

And we can finally use like this.

```rust
fn main() {
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
    
    items.remove(&i1);

    {
        let p1 = items.get(&i1);
        match p1 {
            Some(v) => println!("value: {} {} {}", v.x, v.y, v.z),
            None => println!("None")
        }
    }
}
```

Our result is as expected:

```
value: 0 0 0
None
```

Let us now move the index in the "remove". Making it a little bit harder to reuse the index later.

We get a compilation error now. Good!

```
error[E0382]: borrow of moved value: `i1`
   --> src\main.rs:172:28
    |
157 |     let i1 = items.push(PlayerPosition{x: 0.0, y: 0.0, z: 0.0});
    |         -- move occurs because `i1` has type `GI`, which does not implement the `Copy` trait
...
169 |     items.remove(i1);
    |                  -- value moved here
...
172 |         let p1 = items.get(&i1);
    |                            ^^^ value borrowed here after move
```

We could derive Copy trait for the index here, but would allow a use that we want to avoid. On the other side, we need to share these indices. So we will derive Clone instead.

```rust
#[derive(Copy)]
struct GI
{
    index: usize,
    generation: u32,
}
```

Now we can clone the index and pass to whoever needs it.

```rust
#[derive(Clone)]
struct GI
{
    index: usize,
    generation: u32,
}
```

One last possible improvement, that we will not take, is to implement the traits Index and IndexMut for our GenerationVec and allow "items[i]" access. The problem with these traits is that they are very restrictive and will allow bad use cases. For example:

```rust
use std::ops::{Index, IndexMut};

impl<T: Clone> Index<GI> for GenerationVec<T>
{
    type Output = Option<&T>;
    fn index<'a>(&'a self, i: GI) -> &'a Option<&T>
    {
        
    }
}
```

The first limitation is that the "index" method is defined to return &Output. It must always be a reference. In our case this is a problem in one of the three cases:

1 - Data exists and generation are equal - ok;  
2 - Data does not exist - ok;
3 - Data exists but generations are different - nok.

In this third case we were returning a "None", but now I need to return a reference. See that "get" returns a Option<> and not &Option<>. But worst part is the "write". I would need to return a &mut to a item, possibly empty, that would have to be activated when a value is set. That would be very hard to do in this API.
So it is probably better not to use this API at all.

To update the value we are going to need two functions. "set" is expected to be used when you have a valid index to an item. If your index became stale we return None. The other, "set_if_none" is expected to be used to populate the position for the first time. We pass a simple int and if it is none, we return a valid index.

```rust
impl<T: Clone> GenerationVec<T>
{
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
```

This allow us a very safe use of the Vec.

```rust
    // We simulate the player is deleted.
    items.remove(i1.clone());

    // Here we simulate that we try to get the players position.
    // We going to receive a None, signaling that our "index" is stale.
    {
        let p1 = items.get(&i1);
        match p1 {
            Some(v) => println!("value: {} {} {}", v.x, v.y, v.z),
            None => println!("None")
        }
    }

    // Here we simulate that you want to update the position of a already
    // delete player. You will receive None.
    let setr1 = items.set(&i1, PlayerPosition{x: 0.0, y: 10.0, z: 0.0});
    println!("Set Result None: {}", setr1.is_none());

    //But you can fill the Vec position for first time after deletion using
    //a simple usize, if the tposition is None.    
    let setr2 = items.set_if_none(0, PlayerPosition{x: 0.0, y: 10.0, z: 0.0});
    println!("Set Result None: {}", setr2.is_none());
    
    //Here we simulate the use of both indices:
    // the old return none
    // the new return the value we "put" above.
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
```
