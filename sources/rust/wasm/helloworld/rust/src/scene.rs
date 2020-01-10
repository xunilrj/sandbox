pub trait Entity
{

}

pub struct Camera
{

}

pub struct Scene
{
    entities: Vec<Box<dyn Entity>>,
    cameras: Vec<Box<Camera>>,
}

impl Scene
{
    pub fn push_entity(&mut self, entity: Box<dyn Entity>) -> &mut dyn Entity
    {
        self.entities.push(entity);
        self.entities.last_mut()
            .unwrap()
            .as_mut()
    }

    pub fn push_camera(&mut self, cam: Box<Camera>) -> &mut Camera
    {
        self.cameras.push(cam);
        self.cameras.last_mut()
            .unwrap()
            .as_mut()
    }
}