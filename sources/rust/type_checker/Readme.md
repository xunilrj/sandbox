# Wiring a Type Cheker in Rust (without all the math fuzz)

Our little type checker will be based on the seminal paper "Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism" (https://arxiv.org/abs/1306.6032).

Unfortunately the paper is focused on people with math fluency, which makes some of the notation used inpenetrable for some. I believe that expressed in other terms, the paper and the notation can be easily interpreted.

But to start, we will start with a simpler case, like David Raymond Christiansen did at "Bidirectional Typing Rules: A Tutorial" (https://davidchristiansen.dk/tutorials/bidirectional.pdf). Where we can only manipulate booleans. Luckly we will be able to generalize our type checker into something more interesting later.

So let us focus at section 1.1 of Christiansen tutorial. He proposes the wording `Terms` as below, but we are going to call it `Expression`.

```rust
// Terms:
// t ::= x, y, z, . . . Variables
// | t t Application
// | λ x . t Abstraction
// | true | false Boolean constants
// | if t then t else t Conditional expressions
pub enum Expression {
    Variable { name: String },
    Application { function: Box<Expression>, argument: Box<Expression> },
    Abstraction { parameter: String, body: Box<Expression> },
    Constant { value: bool },
    Conditional { test: Box<Expression>, then_body: Box<Expression>, else_body: Box<Expression> }
}
```

And he proposes `Types` but we are going to use `Type` in singular :

```rust
// Types:
// τ ::= Bool Boolean type
// | τ → τ Function type
#[derive(Clone)]
pub enum Type {
    Boolean,
    Function { argument: Box<Type>, result: Box<Type> }
}
```

Both are simpler enough. We can only have two types: booleans and functions. Functions are recursive, but we are going to ignore this for now and assume function can only be from booleans to booleans, or as people like to put it: `bool -> bool`.

With this we can start our inference algorithm. We are going to model this a little bit different than Christiansen did. He is following a more functional approach, we are going to bring this closer to Rust.

```rust
#[derive(Default)]
pub struct TypeChecker {
}

impl TypeChecker {
    pub fn infer(&mut self, expr: &Expression) -> Option<Type> {
        todo!()
    }
}
```

To simplify we are going to call this `TypeChecker`, although it also do inference. Another point is that in the bidirectional typing world, inference is actually called `synthetise`. We are going to keep inference because it seems simpler.

## Constants

So the first case and the simplest is to infer the type of constants:

```rust
pub fn infer(&mut self, expr: &Expression) -> Option<Type> {
    match expr {
        Expression::Constant { .. } => Some(Type::Boolean),
        _ => todo!()
    }
}
```

There is really no explanation here. Every constant, in our simple case, is of type `bool`. We can test it to guarantee:

```rust
#[test]
pub fn ok_infer_constant_bool() {
    let mut tc = TypeChecker::default();
    let t = tc.infer(&Expression::Constant { value: true });
    assert!(matches!(t, Some(Type::Boolean)));
}
```

If we go back to Christiansen's paper we will see this rule written as:

$$
\frac{}{\Gamma \vdash \textnormal{true} \implies \textnormal{Bool} }
$$

$$
\frac{}{\Gamma \vdash \textnormal{false} \implies \textnormal{Bool} }
$$

and should be read as, "without any precondition" , nothing above the line, "we can say that `true` has type `Bool` given context `Gamma`”. Same thing for `false`, of course.

## Variables

The next obvious step is to infer variables. This case will be slight more interesting because we will have to deal with the "context". In our case, we will not model the algorithm like they normally do in mathematical and functional programming, that is with everything immutable.

We will actually model the context as our `TypeChecker` struct itself. So, if we read the rule for variable inference:

$$
\frac{(x:\tau) \in \Gamma}{\Gamma \vdash \textnormal{x} \implies \tau }
$$

Which can read as: "with the pre-condition that `x` typed as `tau` is inside the context, then we can say that `x` has type `tau`".

In Rust we can do:

```rust
pub fn infer(&mut self, expr: &Expression) -> Option<Type> {
    match expr {
        Expression::Constant { .. } => Some(Type::Boolean),
        Expression::Variable { name } => self.get_variable_type(&name),
        _ => todo!(),
    }
}

fn get_variable_type(&self, name: &str) -> Option<Type> {
    let var = self.variables.get(name)?;
    Some(var.clone())
}
```

And to test this we can do:

```rust
#[test]
pub fn ok_infer_variable() {
    let mut tc = TypeChecker::default();
    tc.variables.insert("x".into(), Type::Boolean);
    let t = tc.infer(&Expression::Variable { name: "x".into() });
    assert!(matches!(t, Some(Type::Boolean)));
}
```

# If and Else

This one, has a pretty huge rule, but it is relatively simple to understand

$$
\frac{\Gamma \vdash t_1: \textnormal{Bool} \space\space\space\space \Gamma \vdash t_2: \tau \space\space\space\space \Gamma \vdash t_3: \tau}{\Gamma \vdash \textnormal{if} \space\space t_1 \space\space \textnormal{then} \space\space t_2 \space\space \textnormal{else} \space\space t_3 : \tau}
$$

"with the pre-condition that the context implies `t1`being a bool, `t2` being of any type here named `tau`, `t3` also being of this same type, we can infer that an if expression will have type `tau`."

So, this tell us that an `if` works on bool variable and each of its branch should return the same type.

Interesting to note that here, `if` are expressions. Not all languages model `if` like this.

To implement this, we can simple do

```rust
Expression::Conditional {
    test,
    then_body,
    else_body,
} => {
    let test_type = self.infer(test.as_ref())?;
    if !test_type.is_bool() {
        return None;
    }

    let then_type = self.infer(then_body.as_ref());
    let else_type = self.infer(else_body.as_ref());

    match (then_type, else_type) {
        (Some(then_type), Some(else_type)) if then_type == else_type => Some(then_type),
        _ => None,
    }
}
```

And we can write a simple test like:

```rust
#[test]
pub fn ok_infer_if_else() {
    let mut tc = TypeChecker::default();
    let t = tc.infer(&Expression::Conditional {
        test: Box::new(Expression::Constant { value: true }),
        then_body: Box::new(Expression::Constant { value: true }),
        else_body: Box::new(Expression::Constant { value: true }),
    });
    assert!(matches!(t, Some(Type::Boolean)));
}
```

# Application

Application prettu much means calling a function. Maybe this will be easier to understand with this unit test where we have one variable `f`, which is actually a function that receives a bool and return a bool. Maybe you don´t like a function being called a variable, but remember that the type chekcer is language agnostic. This jsut mean something called "f" is a `bool -> bool` function.

And we also have a variable `x` which is a simple bool.

When we call `f(x)` we expect a `bool`. That is what we are testing here.

```rust
#[test]
pub fn ok_infer_application() {
    let mut tc = TypeChecker::default();
    tc.variables.insert(
        "f".into(),
        Type::Function {
            argument: Box::new(Type::Boolean),
            result: Box::new(Type::Boolean),
        },
    );
    tc.variables.insert("x".into(), Type::Boolean);
    let t = tc.infer(&Expression::Application {
        function: Box::new(Expression::Variable { name: "f".into() }),
        argument: Box::new(Expression::Variable { name: "x".into() }),
    });
    assert!(matches!(t, Some(Type::Boolean)));
}
```

The implementation is also quite simple. Just resolve the `function` and hope it resolves to a function. If not, one may decide to implement one `callable` things in your language. Here we just fail.

Then we check the arguments resolves to the same type as the function is expecting. If yes we return the function return type.

```rust
Expression::Application { function, argument } => {
    let arg_type = self.infer(argument.as_ref())?;

    let f_type = self.infer(function.as_ref())?;
    match f_type {
        Type::Function { argument, result } => {
            if &arg_type != argument.as_ref() {
                return None;
            }

            Some(result.as_ref().clone())
        }
        _ => return None,
    }
}
```
