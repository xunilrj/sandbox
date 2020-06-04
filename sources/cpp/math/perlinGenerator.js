import Perlin from 'perlin.js';

export function perlinGenerator(geometry, freqs = 8, strength = 10)
{
  let array = geometry.attributes.position.array;
  let xstride = geometry.parameters.widthSegments + 1;
  let ystride = geometry.parameters.heightSegments + 1;

  Perlin.seed(Math.random());

  let p = freqs;
  for(let x = 0;x <= xstride; ++x)
  {
    for(let y = 0;y <= ystride; ++y)  
    {
      array[x*xstride*3 + y*3 + 2] = 
        Perlin.perlin2( x/(2*p), y/( 2*p)) * strength +
        Perlin.perlin2( x/(4*p), y/( 4*p)) * strength +
        Perlin.perlin2( x/(8*p), y/( 8*p)) * strength +
        Perlin.perlin2(x/(16*p), y/(16*p)) * strength;
    }
  }

  geometry.attributes.position.needsUpdate = true;
}