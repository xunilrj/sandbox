# Introduction

Monadic programming in C# using async/await as the "monadic" operator.

## Maybe

The Maybe Monad allows you to specify that a value does not contains a value. Very similar to Nullable<T> but with a safer API that
avoid throwing exceptions.

We also have a special behaviour in async/await methods that short circuit the method avoiding the necessity of "ifs".

### Simple Structure

  struct Maybe<T>  
  {  
        bool IsEmpty;  
        T Value;  
  } 
  
So one have all the possible values of T (possibly including null), new Maybe<T>(isEmpty: false, value: GetSomeValue<T>()) and one
more value new Maybe<T>(isEmpty: true). That is what the Haskell documentation call the Type Equation as F + 1. The plus one means 
one more point the { IsEmpty = True } point. 

### See

https://wiki.haskell.org/Maybe

### Possible Name

* Nullable<T>  
* Definable<T> (apparently there is no Undefinable in English)  
* Undefinable<T> (neologism)  
* Maybe<T>  
* Option<T>  
* Emptiable<T>  (probably another neologism)

### Todo

* Implicit cast from (bool, T)
* More Binds from Tuples
* Awaiter for Tuple of Maybes
* Allow inversion of Maybe for await (only continue when Empty)
* Allow lifting functions
* Allow binding to objects
