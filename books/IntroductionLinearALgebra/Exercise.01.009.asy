size(78cm,20cm);
import geometry;

show(defaultcoordsys);

var a=(1,1);
var b=(4,2);
var c=(1,3);
var d=(0,0);

dot(a, black);
label("A", a, S);
dot(b, black);
label("B", b, E);
dot(c, black);
label("C", c, N);

var PA = a+c-b;
dot(PA, red);
label((string)PA, PA, W);
draw(a--b--c--PA--a, red+dashed);

var PB = a+b-c;
dot(PB, red);
label((string)PB, PB, S);
draw(a--c--b--PB--a, blue+dashed);

var PC = c+b-a;
dot(PC, red);
label((string)PC, PC, N);
draw(a--b--PC--c--a, green+dashed);