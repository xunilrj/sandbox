import three;
import grid3;

size(8cm,0);
currentprojection=orthographic(1,0,0);
limits((-10,-10,0),(10,10,10));
xaxis3("$x$", Arrow3);
yaxis3("$y$", Arrow3);
zaxis3("$z$", Arrow3);

var A = (1,0,0);
var B = (0,2,3);

draw(box(-2*A, +2*A+B), black+linewidth(0.6pt)+dashed);
draw(box(O, A), black+linewidth(0.6pt)+dashed);
draw(box(O, B), black+linewidth(0.6pt)+dashed);

dot(Label("",align=Z), A, black);
dot(Label("",align=Z), B, black);

path3 PA = plane(4*A,4*B,-2*A-2*B);
draw(PA, blue);

srand(0);
for(var x = 0;x <= 1000; ++x)
{
    var a = unitrand()*2;
    var b = unitrand()*2;
    dot(a*A+b*B, red+linewidth(1.0pt));
}