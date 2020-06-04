export function randomGenerator(geometry, strength = 0.3)
{
  let xstride = geometry.parameters.widthSegments + 1;
  let ystride = geometry.parameters.heightSegments + 1;
  
  let array = geometry.attributes.position.array;
  let accum = 0;
  for(let x = 0;x <= xstride; ++x)
  {
    for(let y = 0;y <= ystride; ++y)
    {
      accum += (Math.random() * 2 - 1) * strength;      
      array[x*xstride*3 + y*3 + 2] = accum;
    }
  }
  
  for(let y = 0;y <= ystride; ++y)  
  {
    for(let x = 0;x <= xstride; ++x)
    {
      accum += (Math.random() * 2 - 1) * strength;      
      array[x*xstride*3 + y*3 + 2] += accum;
    }
  }

  geometry.attributes.position.needsUpdate = true;
}