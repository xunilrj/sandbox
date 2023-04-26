use std::collections::HashMap;

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    Boolean,
    Function {
        argument: Box<Type>,
        result: Box<Type>,
    },
}

impl Type {
    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Boolean)
    }
}

pub enum Expression {
    Variable {
        name: String,
    },
    Application {
        function: Box<Expression>,
        argument: Box<Expression>,
    },
    Abstraction {
        parameter: String,
        body: Box<Expression>,
    },
    Constant {
        value: bool,
    },
    Conditional {
        test: Box<Expression>,
        then_body: Box<Expression>,
        else_body: Box<Expression>,
    },
}

#[derive(Default)]
pub struct TypeChecker {
    variables: HashMap<String, Type>,
}

impl TypeChecker {
    fn get_variable_type(&self, name: &str) -> Option<Type> {
        let var = self.variables.get(name)?;
        Some(var.clone())
    }

    pub fn infer(&mut self, expr: &Expression) -> Option<Type> {
        match expr {
            Expression::Constant { .. } => Some(Type::Boolean),
            Expression::Variable { name } => self.get_variable_type(&name),
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
            _ => todo!(),
        }
    }
}

#[test]
pub fn ok_infer_constant_bool() {
    let mut tc = TypeChecker::default();
    let t = tc.infer(&Expression::Constant { value: true });
    assert!(matches!(t, Some(Type::Boolean)));
}

#[test]
pub fn ok_infer_variable() {
    let mut tc = TypeChecker::default();
    tc.variables.insert("x".into(), Type::Boolean);
    let t = tc.infer(&Expression::Variable { name: "x".into() });
    assert!(matches!(t, Some(Type::Boolean)));
}

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

fn main() {
    println!("Hello, world!");
}
