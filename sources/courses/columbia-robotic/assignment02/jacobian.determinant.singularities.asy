import graph3;
texpreamble("\usepackage{icomma}");

real detJ(pair z) {
  return sin(z.x)*z.y*z.y + 2*z.y;
}

currentprojection=orthographic(1,-2,0.5);

draw(surface(detJ,(-10,-10),(5,5),nx=100, Spline), lightgray);

if(!is3D())
  shipout(bbox(3mm,Fill(black)));
