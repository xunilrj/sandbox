size(78cm,20cm);
import geometry;

show(defaultcoordsys);

var v=(4,2);
var w=(-1,2);

draw("$\vec{v}$", origin()--origin()+v, Arrow(3mm));
draw("$\vec{w}$", origin()--origin()+w, Arrow(3mm));
draw("", v--v+w, dashed);
draw("", w--w+v, dashed);

draw(Label("$\vec{v+w}$", 0.4), origin()--origin()+(v+w), Arrow(3mm));
draw(Label("$\vec{v-w}$", 0.4), w--(w+v-w), Arrow(3mm));

draw("", (v+w)--(v+w+v-w), dashed);
draw(Label("$\vec{(v+w)+(v-w)}$", 0.6), origin()--(v+w+v-w), red,Arrow(3mm));


