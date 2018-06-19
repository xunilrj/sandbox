import three;
import grid3;

size(8cm,0);
currentprojection=orthographic(1,1,1);
limits((0,-10,0),(10,10,10));
xaxis3("$x$", Arrow3);
yaxis3("$y$", Arrow3);
zaxis3("$z$", Arrow3);

var A = (1,2,3);
var B = (3,6,9);

triple fshadow(real t) { return t*(1,2,0); }
triple f(real t) { return t*A; }
draw(graph(f, -10, 10, operator ..), p=blue);
draw(graph(fshadow, 0, 9, operator ..), p=black+dashed);
draw(box(O, A), black+linewidth(0.6pt)+dashed);
draw(box(O, B), black+linewidth(0.6pt)+dashed);
dot(Label("",align=Z), A, black);
dot(Label("",align=Z), B, black);

srand(0);
for(var x = 0;x <= 1000; ++x)
{
    var a = unitrand();
    var b = unitrand();
    dot(a*A+b*B, red+linewidth(1.0pt));
}