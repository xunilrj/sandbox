# Minimal RustyTyC example

`RustyTyC` is a type checker and resolver, which means that you define type constraints and bounds, and ask the system to solve it. The result is going to be the best possible solution given the rules you specified.

For example:

```rust
fn main() {
    let mut system: VarlessTypeChecker<TypeCheckerTypes> = TypeChecker::without_vars();

    let variable_a = system.new_term_key();
    let rule1 = variable_a.concretizes_explicit(TypeCheckerTypes::Integer(5));
    system.impose(rule1);

    let r = system.type_check();
    dbg!(r);
}
```

The result we get back is:

```
> cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.88s
     Running `target\debug\typechecker.exe`
[src\main.rs:86] r = Ok(
    {
        TcKey {
            ix: 0,
        }: Int128,
    },
)
```

```Ok(...)``` means that the system managed to find a solution to all variables. In this particular case, because we used ```impose```; which constraint which types a variable can assume.

If we remove the impose, commenting that line, and run again, we get back:

```
> cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 1.08s
     Running `target\debug\typechecker.exe`
[src\main.rs:86] r = Err(
    Construction(
        TcKey {
            ix: 0,
        },
        Preliminary {
            TypeCheckerTypes: Any,
            children: [],
        },
        "Cannot reify `Any`.",
    ),
)
```

We got an ```Err(...)``` back; in this case we cannot "reify" the variable index 0. This means that ```variable_a``` started as ```Any``` and with no other tips about it, the type checker could only fail.

One question that his brings is: "isn't failing for Any a little bit agrresive? Maybe Any is fine for me because I want to give an warning, or squiggle the variable on the editor...."

And the answer is "Yes! It is". Luckly we can change that. The type checker does not force you to ```Err```on Any. It actually asks you what you want to do with any.

## Making Any a viable solution

To configure this you can look into the trait ```Constructable```. Ignoring the implementation for now, let us just log this method. Also ignore what ```TypeCheckerTypes``` is for now.

```rust
impl Constructable for TypeCheckerTypes {
    ...

    fn construct(&self, children: &[Self::Type]) -> Result<Self::Type, Self::Err> {
        trace!(target: "<TypeCheckerTypes as Constructable>::construct" ,"{:?} {:?}", self, children);
        ...
    }
}
```

If we run now, we get:

```
> cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 1.06s
     Running `target\debug\typechecker.exe`
 TRACE <TypeCheckerTypes as Constructable>::construct > Any []
[src\main.rs:91] r = Err(
    Construction(
        TcKey {
            ix: 0,
        },
        Preliminary {
            variant: Any,
            children: [],
        },
        "Cannot reify `Any`.",
    ),
)
```

So the type checker call ```Constructable::construct``` once with ```Any```. If we return our ```impose```.


```
> cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 1.23s
     Running `target\debug\typechecker.exe`
 TRACE <TypeCheckerTypes as Constructable>::construct > Integer(5) []
[src\main.rs:91] r = Ok(
    {
        TcKey {
            ix: 0,
        }: Int128,
    },
)
```

So now it is calling ```Constructable::construct``` once with ```Integer(5)``` which by no coincidence, is what we imposed into the system.

Which means that the type checker is calling ```Constructable``` trait with its best solution. If we peek into more details of this implementation, we will find out how to avoid returning an err when the best solution for a variable is ```Any```.

```rust
impl Constructable for TypeCheckerTypes {
    type Type = FinalTypes;

    fn construct(&self, children: &[Self::Type]) -> Result<Self::Type, Self::Err> {
        trace!(target: "<TypeCheckerTypes as Constructable>::construct" ,"{:?} {:?}", self, children);
        use TypeCheckerTypes::*;
        match self {
            Any => Err("Cannot reify `Any`.".to_string()),
            ...
        }
    }
}
```

Calling this trait, the type checker is allowing us to map from all possible types from the type resolution to the concrete types that we are actually interested.

```TypeCheckerTypes``` here is the set of all possible types that are interesting to the type checker in the format that is best suited to run the algorithm. We can dive into it later.

```FinalTypes``` is the set the types that we are interested. In the end we want the algorithm to say: "Hey, variable_a is a integer".

These two types were actually created by me. And we will see their complete definition later. For now let us ignore them.

In this case, we are still ignoring all "match arms" that do this conversion, because the very first arm is the most interesting for us now. It is telling the type checker that if a type at the end of the solution in ```Any```, so return an ```Err```.

The first thing we can do to accept ```Any``` as final solution is:

```rust
impl Constructable for TypeCheckerTypes {
    type Type = FinalTypes;

    fn construct(&self, children: &[Self::Type]) -> Result<Self::Type, Self::Err> {
        trace!(target: "<Variant as Constructable>::construct" ,"{:?} {:?}", self, children);        
        use TypeCheckerTypes::*;
        match self {
            Any => Ok(FinalTypes::Any),
            ...
        }
    }
}
```

If we run now (remembering of commenting the ```impose```), we have:

```
> cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 1.72s
    Running `target\debug\typechecker.exe`
 TRACE <Variant as Constructable>::construct > Integer(5) []
[src\main.rs:91] r = Ok(
    {
        TcKey {
            ix: 0,
        }: Any,
    },
)
```

Much better! ```Ok``` and ```Any```.

# Understanding the set of possible sets