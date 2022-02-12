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
        trace!(target: "<TypeCheckerTypes as Constructable>::construct" ,"{:?} {:?}", self, children);        
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
 TRACE <TypeCheckerTypes as Constructable>::construct > Integer(5) []
[src\main.rs:91] r = Ok(
    {
        TcKey {
            ix: 0,
        }: Any,
    },
)
```

Much better! ```Ok``` and ```Any```.

## How rusttyc deal with types

The type checker is flexible in the set of types it can work on. Actually it knows nothing about this set. It only knows that these types can have some standard relation ships. It is up to us to define not only these relationships, but the types themselves.

To define this, we create a simple enum:

```rust
#[derive(Debug, Clone, Copy, PartialOrd, PartialEq, Ord, Eq, Hash)]
enum TypeCheckerTypes {
    Any,
    Fixed(u8, u8),
    Integer(u8),
    Numeric,
    Bool,
}
```

It really does not matter what you put here, it is up to you and whatever you are modelling. The important part is that this enum must implement ```Variant```.

```rust
impl Variant for TypeCheckerTypes {
    ...
}
```

First we need to define what is the most abstract possible type. In our case this is easy: ```TypeCheckerTypes```. 

```rust
impl Variant for TypeCheckerTypes {
    ...
    fn top() -> Self {
        Self::Any
    }
    ...
}
```

The season why the method is called ```top```and not ```get_most_abstract_variant``` is because of the algorithm used to solve all the constraints, which we can try to dive into later.

If we log when this method is called...

```rust
impl Variant for TypeCheckerTypes {
    ...
    fn top() -> Self {
        trace!(target: "<TypeCheckerTypes as Variant>::top", "");
        Self::Any
    }
    ...
}
```

we get

```rust
> cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.90s
    Running `target\debug\typechecker.exe`
 TRACE <TypeCheckerTypes as Variant>::top > 
 TRACE <TypeCheckerTypes as Constructable>::construct > Integer(5) []
[src\main.rs:89] r = Ok(
    {
        TcKey {
            ix: 0,
        }: Int128,
    },
)
```

Which makes sense. First the type checker calls ```Variant::top``` to initialize all variables and at the end it tell us its conclusions and it asks us to convert from ```TypeCheckerTypes``` to our final type ```FinalTypes```. Whice is simply an enum will all possible types that we want to see at the end. For example

```rust
#[derive(Debug, Clone, Copy, PartialOrd, PartialEq, Ord, Eq, Hash)]
enum FinalTypes {
    Any,
    Int128,
    FixedPointI64F64,
    Bool,
}
```

If our language support parametrized types, we could have them here. What is important is that the bridge between these types is also define by us at the ```Constructable``` trait. Now we can understand its complete implementation.

```rust
impl Constructable for TypeCheckerTypes {
    type Type = FinalTypes;

    fn construct(&self, children: &[Self::Type]) -> Result<Self::Type, Self::Err> {
        trace!(target: "<TypeCheckerTypes as Constructable>::construct" ,"{:?} {:?}", self, children);
        use TypeCheckerTypes::*;
        match self {
            Any => Ok(FinalTypes::Any),
            Integer(w) if *w <= 128 => Ok(FinalTypes::Int128),
            Integer(w) => Err(format!("Integer too wide, {}-bit not supported.", w)),
            Fixed(i, f) if *i <= 64 && *f <= 64 => Ok(FinalTypes::FixedPointI64F64),
            Fixed(i, f) => Err(format!("Fixed point number too wide, I{}F{} not supported.", i, f)),
            Numeric => {
                Err("Cannot reify a numeric value. Either define a default (int/fixed) or restrict type.".to_string())
            }
            Bool => Ok(FinalTypes::Bool),
        }
    }
}
```

## What happens when we impose a type

This is all fine and good. But we saw at the beggining that we need to impose something into the type checker to allow it to reify our ```variable_a``` into something more concrete than ```Any```.

Given that the type checker does not know the set of all possible types. And it does not know its relationships. Would be impossible for it to know if ```Integer(5)``` is "better" than ```Any```. Even if it is "better" than ```Integet(4)``` or ```Integer(6)```.

Actually, all this is opaque to it. We have to help it somehow.

This help come from the implementation of ```Variant::meet```. This method receives two parameters, that we can understand better by tracing them.

```rust
impl Variant for TypeCheckerTypes {
    ...
    fn meet(lhs: Partial<Self>, rhs: Partial<Self>) -> Result<Partial<Self>, Self::Err> {
        trace!(target: "<TypeCheckerTypes as Variant>::meet", "{:?} {:?}", &lhs, &rhs);
        ...
    }
    ...
}
```

If we run now we get:

```
> cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 1.03s
    Running `target\debug\typechecker.exe`
 TRACE <TypeCheckerTypes as Variant>::top > 
 TRACE <TypeCheckerTypes as Variant>::meet > Partial { variant: Any, least_arity: 0 } Partial { variant: Integer(5), least_arity: 0 }
 TRACE <TypeCheckerTypes as Constructable>::construct > Integer(5) []
[src\main.rs:89] r = Ok(
    {
        TcKey {
            ix: 0,
        }: Int128,
    },
)
```

First thing to notice is that ```Variant::meet``` is called after ```Variant::top``` and before ```Constructable::construct```. Second is that the ```lhs``` parameter has a variant ```Any``` and ```rhs``` parameter has a value of ```Integer(5)```. The initial value and the imposed value of ```variable_a```. No coincidences here, of course.

The type checker found a "impose rule" and is calling ```Variant::meet``` to try to satisfy this rule. Is this particular case the implementation is quite easy, because we can transition from ```Any``` to any other type.

```rust
impl Variant for TypeCheckerTypes {
    ...
    fn meet(lhs: Partial<Self>, rhs: Partial<Self>) -> Result<Partial<Self>, Self::Err> {
        trace!(target: "<TypeCheckerTypes as Variant>::meet", "{:?} {:?}", &lhs, &rhs);
        use TypeCheckerTypes::*;
        let variant = match (lhs.variant, rhs.variant) {
            (Any, other) | (other, Any) => Ok(other),
            ...
        }?;
        Ok(Partial {
            variant,
            least_arity: 0,
        })
    }
}
```

Important to realise that this method returns ```Result<...>```. It can fail. This means that if we find a contradiction, the type checker will signal this to us. And it does this in two possible ways.

Fist when we call ```TypeChecker::type_check(...)```. This methods returns a ```Result<...>``` and two interesting variants of the ```Err``` side are: ```KeyEquation``` and ```Bound```.

This second one is very easy to get. We just need to impose two contradictory rules, for example.

```rust
fn main() {
    pretty_env_logger::init();

    let mut system: VarlessTypeChecker<TypeCheckerTypes> = TypeChecker::without_vars();

    let variable_a = system.new_term_key();

    let rule1 = variable_a.concretizes_explicit(TypeCheckerTypes::Bool);
    dbg!(system.impose(rule1));

    let rule2 = variable_a.concretizes_explicit(TypeCheckerTypes::Integer(5));
    dbg!(system.impose(rule2));
}
```

We not even need to call the ```type_check``` method, because in this case the type checker will realise as soon as we impose the contradiction, tha something is fishy.

```
> cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.98s
    Running `target\debug\typechecker.exe`
 TRACE <TypeCheckerTypes as Variant>::top > 
 TRACE <TypeCheckerTypes as Variant>::meet > Partial { variant: Any, least_arity: 0 } Partial { variant: Bool, least_arity: 0 }
 TRACE typechecker                         > Ok(())
 TRACE <TypeCheckerTypes as Variant>::meet > Partial { variant: Bool, least_arity: 0 } Partial { variant: Integer(5), least_arity: 0 }
 TRACE typechecker                         > Err(Bound(TcKey { ix: 0 }, None, "bool can only be combined with bool"))
 ```

The last line is the interesting one, and it come from our code. In the ```Variant::meet``` method we say that we can't "meet" integer and booleans.

```rust
impl Variant for TypeCheckerTypes {
    ...
    fn meet(lhs: Partial<Self>, rhs: Partial<Self>) -> Result<Partial<Self>, Self::Err> {
        trace!(target: "<TypeCheckerTypes as Variant>::meet", "{:?} {:?}", &lhs, &rhs);
        use TypeCheckerTypes::*;
        let variant = match (lhs.variant, rhs.variant) {
            (Any, other) | (other, Any) => Ok(other),
            ...
            (Bool, Bool) => Ok(Bool),
            (Bool, _) | (_, Bool) => Err("bool can only be combined with bool"),
            ...
        }?;
        Ok(Partial {
            variant,
            least_arity: 0,
        })
    }
    ...
}
```

A second possibility is when we introduce two variables; we say that ```variable_a``` is ```Bool```; ```variable_b``` is ```Integer(5)```.


```rust
fn main() {
    pretty_env_logger::init();

    let mut system: VarlessTypeChecker<TypeCheckerTypes> = TypeChecker::without_vars();

    let variable_a = system.new_term_key();
    let rule1 = variable_a.concretizes_explicit(TypeCheckerTypes::Bool);
    trace!("{:?}", system.impose(rule1));

    let variable_b = system.new_term_key();
    let rule2 = variable_b.concretizes_explicit(TypeCheckerTypes::Integer(5));
    trace!("{:?}", system.impose(rule2));

    let rule3 = variable_a.concretizes(variable_b);
    trace!("{:?}", system.impose(rule3));

    let r = system.type_check();
    dbg!(r);
}
```

It is interesting to realize that imposing ```rule3``` is ok. The type checker will see not immediate contradiction. It will only be realized in the ```type_check``` method.

```
> cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.90s
    Running `target\debug\typechecker.exe`
 TRACE <TypeCheckerTypes as Variant>::top >
 TRACE <TypeCheckerTypes as Variant>::meet > Partial { variant: Any, least_arity: 0 } Partial { variant: Bool, least_arity: 0 }
 TRACE typechecker                         > Ok(())
 TRACE <TypeCheckerTypes as Variant>::top  >
 TRACE <TypeCheckerTypes as Variant>::meet > Partial { variant: Any, least_arity: 0 } Partial { variant: Integer(5), least_arity: 0 }
 TRACE typechecker                         > Ok(())
 TRACE typechecker                         > Ok(())
TypeChecker::type_check
 TRACE <TypeCheckerTypes as Variant>::meet > Partial { variant: Bool, least_arity: 0 } Partial { variant: Integer(5), least_arity: 0 }
[src\main.rs:96] r = Err(
    Bound(
        TcKey {
            ix: 0,
        },
        Some(
            TcKey {
                ix: 1,
            },
        ),
        "bool can only be combined with bool",
    ),
)
```

Which is telling me that it failed bounding ```variable_a``` (index 0) and ```variable_b``` (index 1).

This is interesting, because this is the kind of bind rule that can happen in code like 

```
variable_a = true
variable_b = 123
if something { variable_a } else { variable_b }
```

We can now generate a nice error stating that each branch of the "if" needs to return compatible types.

# Theory

https://www.youtube.com/playlist?list=PL5rqYzyihIQ0nzfnsEKxxedCpbNQoifgg
