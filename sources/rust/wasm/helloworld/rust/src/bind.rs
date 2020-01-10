#[cfg(test)]
mod matrix44_tests {
    use super::*;
    use crate::math::*;
    use std::rc::Rc;
    use std::cell::RefCell;
    use std::collections::HashMap;

    pub struct Entity
    {
        pos: Vec3,
    }

    pub struct Camera
    {
        pos: Vec3,
    }

    pub struct Game
    {
        entities: Vec<Entity>,
        cameras: Vec<Camera>,
        HashMap<>
    }

    impl Game
    {
        pub fn bind<T>(&mut self, from: &T, to: &T)
        {

        }

        pub fn set_value<T>(&mut self, from: &mut T, v: T)
        {

        }
    }

    #[test]
    fn matrix44_id_mul_id() {
        let mut g = Game {
            entities: Vec::new(),
            cameras: Vec::new(),
        };
        let e = Entity { pos: Vec3::new(0.0, 0.0, 0.0) };
        //g.entities.push(e);
        let c = Camera { pos: Vec3::new(0.0, 0.0, 0.0) };
        //g.cameras.push(c);

        g.bind(&e.pos, &c.pos);
    }
}