#[derive(Copy, Clone)]
pub struct Vec3
{
    pub x: f32,
    pub y: f32,
    pub z: f32,    
}

impl Vec3
{
    pub fn new(x: f32, y: f32, z: f32) -> Vec3 { Vec3{ x, y, z} }
    pub fn sq_length(&self) -> f32 { ((self.x*self.x) + (self.y*self.y) + (self.z*self.z)) }
    pub fn length(&self) -> f32 { ((self.x*self.x) + (self.y*self.y) + (self.z*self.z)).sqrt() }
    pub fn normalize(&self) -> Vec3 {        
        let l = self.length();
        Vec3::new(self.x/l, self.y/l, self.z/l)
    }
    pub fn dot(&self, r: Vec3) -> f32 { ((self.x*r.x) + (self.y*r.y) + (self.z*r.z)).sqrt() }
    pub fn cross(&self, r: Vec3) -> Vec3 {        
        Vec3::new(
            self.y*r.z - self.z*r.y,
            self.z*r.x - self.x*r.z,
            self.x*r.y - self.y*r.x,
        )
    }
}

impl std::ops::Add for Vec3 { type Output = Vec3; fn add(self, r: Vec3) -> Vec3 { Vec3::new(self.x + r.x, self.y + r.y, self.z + r.z) } }
impl std::ops::Sub for Vec3 { type Output = Vec3; fn sub(self, r: Vec3) -> Vec3 { Vec3::new(self.x - r.x, self.y - r.y, self.z - r.z) } }
impl std::ops::Mul<f32> for Vec3 { type Output = Vec3; fn mul(self, f: f32) -> Vec3 { Vec3::new(self.x * f, self.y * f, self.z * f) } }

pub struct Vec4
{
    x: f32,
    y: f32,
    z: f32,
    w: f32
}

impl Vec4
{
    pub fn new(x: f32, y: f32, z: f32, w:f32) -> Vec4 { Vec4{ x, y, z, w} }
}

pub struct Matrix44
{
    item: [f32;16]
}

impl Matrix44
{
    pub fn id() -> Matrix44
    {
        Matrix44 {
            item: [
                1.0, 0.0, 0.0, 0.0,
                0.0, 1.0, 0.0, 0.0,
                0.0, 0.0, 1.0, 0.0,
                0.0, 0.0, 0.0, 1.0
            ]
        }
    }
}

impl From<[f32;16]> for Matrix44 {
    fn from(v: [f32;16]) -> Matrix44 {
        Matrix44 {
            item: v
        }
    }
}

impl std::ops::Mul for Matrix44 {
    type Output = Matrix44;
    fn mul(self, b: Matrix44) -> Matrix44 {
        let mut r: [f32;16] = [
            0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 0.0
        ];
        r[0]  = self.item[0] * b.item[0]  + self.item[4] * b.item[1]  + self.item[8]  * b.item[2]  + self.item[12] * b.item[3];
        r[1]  = self.item[1] * b.item[0]  + self.item[5] * b.item[1]  + self.item[9]  * b.item[2]  + self.item[13] * b.item[3];
        r[2]  = self.item[2] * b.item[0]  + self.item[6] * b.item[1]  + self.item[10] * b.item[2]  + self.item[14] * b.item[3];
        r[3]  = self.item[3] * b.item[0]  + self.item[7] * b.item[1]  + self.item[11] * b.item[2]  + self.item[15] * b.item[3];
    
        r[4]  = self.item[0] * b.item[4]  + self.item[4] * b.item[5]  + self.item[8]  * b.item[6]  + self.item[12] * b.item[7];
        r[5]  = self.item[1] * b.item[4]  + self.item[5] * b.item[5]  + self.item[9]  * b.item[6]  + self.item[13] * b.item[7];
        r[6]  = self.item[2] * b.item[4]  + self.item[6] * b.item[5]  + self.item[10] * b.item[6]  + self.item[14] * b.item[7];
        r[7]  = self.item[3] * b.item[4]  + self.item[7] * b.item[5]  + self.item[11] * b.item[6]  + self.item[15] * b.item[7];
    
        r[8]  = self.item[0] * b.item[8]  + self.item[4] * b.item[9]  + self.item[8]  * b.item[10] + self.item[12] * b.item[11];
        r[9]  = self.item[1] * b.item[8]  + self.item[5] * b.item[9]  + self.item[9]  * b.item[10] + self.item[13] * b.item[11];
        r[10] = self.item[2] * b.item[8]  + self.item[6] * b.item[9]  + self.item[10] * b.item[10] + self.item[14] * b.item[11];
        r[11] = self.item[3] * b.item[8]  + self.item[7] * b.item[9]  + self.item[11] * b.item[10] + self.item[15] * b.item[11];
    
        r[12] = self.item[0] * b.item[12] + self.item[4] * b.item[13] + self.item[8]  * b.item[14] + self.item[12] * b.item[15];
        r[13] = self.item[1] * b.item[12] + self.item[5] * b.item[13] + self.item[9]  * b.item[14] + self.item[13] * b.item[15];
        r[14] = self.item[2] * b.item[12] + self.item[6] * b.item[13] + self.item[10] * b.item[14] + self.item[14] * b.item[15];
        r[15] = self.item[3] * b.item[12] + self.item[7] * b.item[13] + self.item[11] * b.item[14] + self.item[15] * b.item[15];
        r.into()
    }
}

impl std::ops::Mul<Vec4> for Matrix44 {
    type Output = Vec4; 
    fn mul(self, b: Vec4) -> Vec4 {
        Vec4 {
            x: self.item[0] * b.x  + self.item[4] * b.y  + self.item[8]  * b.z  + self.item[12] * b.w,
            y: self.item[1] * b.x  + self.item[5] * b.y  + self.item[9]  * b.z  + self.item[13] * b.w,
            z: self.item[2] * b.x  + self.item[6] * b.y  + self.item[10] * b.z  + self.item[14] * b.w,
            w: self.item[3] * b.x  + self.item[7] * b.y  + self.item[11] * b.z  + self.item[15] * b.w,
        }
    } 
}


#[cfg(test)]
mod vec3_tests {
    use super::*;

    #[test]
    fn vec3_add() {
        let r = Vec3::new(1.0,2.0,3.0) + Vec3::new(3.0,2.0,1.0);
        assert_eq!(4.0, r.x);
        assert_eq!(4.0, r.y);
        assert_eq!(4.0, r.z);
    }

    #[test]
    fn vec3_sub() {
        let r = Vec3::new(1.0,2.0,3.0) - Vec3::new(3.0,2.0,1.0);
        assert_eq!(-2.0, r.x);
        assert_eq!(0.0, r.y);
        assert_eq!(2.0, r.z);
    }

    #[test]
    fn vec3_mul() {
        let r = Vec3::new(1.0,2.0,3.0) * 2.0;
        assert_eq!(2.0, r.x);
        assert_eq!(4.0, r.y);
        assert_eq!(6.0, r.z);
    }

    #[test]
    fn vec3_dot() {
        let l = Vec3::new(1.0,2.0,3.0);
        let r = Vec3::new(1.0,2.0,3.0);
        assert_eq!((14.0f32).sqrt(), l.dot(r));
    }

    #[test]
    fn vec3_cross() {
        let r = Vec3::new(1.0,0.0,0.0).cross(Vec3::new(0.0,1.0,0.0));
        assert_eq!(0.0, r.x);
        assert_eq!(0.0, r.y);
        assert_eq!(1.0, r.z);

        let r = Vec3::new(0.0,1.0,0.0).cross(Vec3::new(0.0,0.0,1.0));
        assert_eq!(1.0, r.x);
        assert_eq!(0.0, r.y);
        assert_eq!(0.0, r.z);
    }

    #[test]
    fn vec3_length() {
        let l = Vec3::new(0.0,2.0,0.0);
        assert_eq!(2.0, l.length());
        assert_eq!(4.0, l.sq_length());
    }

    #[test]
    fn vec3_normalize() {
        let l = Vec3::new(0.0,2.0,0.0);
        let n = l.normalize();
        assert_eq!(0.0, n.x);
        assert_eq!(1.0, n.y);
        assert_eq!(0.0, n.z);        
    }
}

#[cfg(test)]
mod matrix44_tests {
    use super::*;

    #[test]
    fn matrix44_id_mul_id() {
        let r = Matrix44::id() * Matrix44::id();

        assert_eq!(1.0, r.item[0]);assert_eq!(0.0, r.item[4]);assert_eq!(0.0, r.item[8]);assert_eq!(0.0, r.item[12]);
        assert_eq!(0.0, r.item[1]);assert_eq!(1.0, r.item[5]);assert_eq!(0.0, r.item[9]);assert_eq!(0.0, r.item[13]);
        assert_eq!(0.0, r.item[2]);assert_eq!(0.0, r.item[6]);assert_eq!(1.0, r.item[10]);assert_eq!(0.0, r.item[14]);
        assert_eq!(0.0, r.item[3]);assert_eq!(0.0, r.item[7]);assert_eq!(0.0, r.item[11]);assert_eq!(1.0, r.item[15]);
    }

    #[test]
    fn matrix44_id_mul_vec4() {
        let r = Matrix44::id() * Vec4::new(1.0, 2.0, 3.0, 4.0);

        assert_eq!(1.0, r.x);
        assert_eq!(2.0, r.y);
        assert_eq!(3.0, r.z);
        assert_eq!(4.0, r.w);
    }
}