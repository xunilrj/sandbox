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
Application { function: Box<Term>, argument: Box<Term> },
Abstraction { parameter: String, body: Box<Term> },
Constant { value: bool },
Conditional { test: Box<Term>, then_body: Box<Term>, else_body: Box<Term> }
}
```

And he proposes `Types` but we are going to use `Type` in singular :

```rust
// Types:
// τ ::= Bool Boolean type
// | τ → τ Function type
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
let t = tc.infer(Expression::Constant(true));
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
Expression::Variable { name } => {
self.get_variable_type(&name)
}
_ => todo!()
}
}

fn get_variable_type(name: &str) -> Option<Type> {
let var = self.variables.get(name)?;
Some(var.ty)
}
```
