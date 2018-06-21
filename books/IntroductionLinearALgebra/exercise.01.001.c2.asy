import three;
import grid3;

size(8cm,0);
currentprojection=orthographic(1,0,0);
limits((0,-10,0),(10,10,10));
xaxis3("$x$", Arrow3);
yaxis3("$y$", Arrow3);
zaxis3("$z$", Arrow3);

var A = (2,0,0);
var B = (0,2,2);
var C = (2,2,3);

draw(box(O, A), lightolive+linewidth(0.6pt)+dashed);
draw(box(O, B), mediumgray+linewidth(0.6pt)+dashed);
draw(box(O, C), black+linewidth(0.6pt)+dashed);

draw(box(O, 2*C), blue+linewidth(0.6pt)+dashed);

dot(Label("",align=Z), A, lightolive);
dot(Label("",align=Z), B, mediumgray);
dot(Label("",align=Z), C, black);

srand(0);
for(var x = 0;x <= 1000; ++x)
{
    var a = unitrand();
    var b = unitrand();
    var c = unitrand();    
    dot(a*A+b*B+c*C, red+linewidth(1.0pt));
}