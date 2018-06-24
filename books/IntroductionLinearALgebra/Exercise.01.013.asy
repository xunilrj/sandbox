size(78cm,20cm);
import geometry;

show(defaultcoordsys);

var angle = 2*pi/12;
var v=(cos(1*angle),sin(1*angle));

draw("$\vec{03:00}$", origin()--(cos(0*angle),sin(0*angle)), Arrow(3mm));
draw("$\vec{02:00}$", origin()--v, Arrow(3mm));
draw("$\vec{01:00}$", origin()--(cos(2*angle),sin(2*angle)), Arrow(3mm));
draw("$\vec{12:00}$", origin()--(cos(3*angle),sin(3*angle)), Arrow(3mm));
draw(Label((string)v, N), v);
draw(unitcircle, dashed);
