size(78cm,20cm);
import geometry;

show(defaultcoordsys);

var v=(3,3);
var w=(2,-2);

draw("$\vec{v}$", origin()--origin()+v, Arrow(3mm));
draw("$\vec{w}$", origin()--origin()+w, Arrow(3mm));

draw("", (1,0)--origin()+(v-w), dashed);
dot((1,0));
label("$(1,0)$", (1,0), S);
draw("", (5,0)--origin()+v+w, dashed);
dot((5,0));
label("$(5,0)$", (5,0), S);
draw("", (3,0)--origin()+v, dashed);
dot((3,0));
label("$(3,0)$", (3,0), S);
draw("", (2,0)--origin()+w, dashed);
dot((2,0));
label("$(2,0)$", (2,0), S);

draw("", (0,5)--origin()+(v-w), dashed);
dot((0,5));
label("$(0,5)$", (0,5), W);
draw("", (0,3)--origin()+(v), dashed);
dot((0,3));
label("$(0,3)$", (0,3), W);
draw("", (0,1)--origin()+(v+w), dashed);
dot((0,1));
label("$(0,1)$", (0,1), W);
draw("", (0,-2)--origin()+(w), dashed);
dot((0,-2));
label("$(0,-2)$", (0,-2), W);

draw("$\vec{v+w}$", origin()--origin()+v+w, Arrow(3mm));
draw("$\vec{v-w}$", origin()--origin()+v-w, Arrow(3mm));

 