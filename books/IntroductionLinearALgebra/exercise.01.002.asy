size(78cm,20cm);
import geometry;

show(defaultcoordsys);

var v=(4,1);
var w=(-2,2);

draw("$\vec{v}$", origin()--origin()+v, Arrow(3mm));
draw("$\vec{w}$", origin()--origin()+w, Arrow(3mm));
draw("", v--origin()+v+w, dashed);
draw("", w--origin()+v+w, dashed);

draw("", origin()--(-w), dashed);
draw("", (-w)--(v-w), dashed);
draw("", v--origin()+v-w, dashed);

draw("$\vec{v+w}$", origin()--origin()+v+w, Arrow(3mm));
draw("$\vec{v-w}$", origin()--origin()+v-w, Arrow(3mm));
 