

type f32 = Number;
type float = f32;
type bool = Boolean;

interface Vec3f
{
  x,y,z: f32;
}

type Vec3 = Vec3f;

//page 3
//Points P satisfy N ⋅ P + c = 0
interface ImplicitLine
{
  normal: Vec3;
  c: float;
}

//Points P satisfy P = U + Vt for some scalar t
interface ExplicitLine
{
  U, V: Vec3;
}

interface INormalizable
{
  isNormalizaed() : bool;
}

type Line = 
  (ImplicitLine | ExplicitLine)
  & INormalizable;

//page 4
interface IHaveCenter
{
  center: Vec3
}

class Circle implements IHaveCenter
{
  center: Vec3
  radius: float
}

//page 5
interface Vec2
{
  x: float;
  y: float;
}

function scale(v: Vec2, f: float) : Vec2
{
  return {x: v.x * f, y: v.y * f};
}

function normalize(v: Vec2) : Vec2
{
  return scale(v, length(v)); 
}

//page?
function distance(l: ImplicitLine, p: Vec3)
{
  const length = length(l.normal);
  const sd = dot(l.normal, p) + l.c;
  return sd / length;
}