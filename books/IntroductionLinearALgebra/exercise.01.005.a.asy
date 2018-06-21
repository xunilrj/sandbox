import three;
import grid3;

size(8cm,0);
currentprojection=orthographic(0.5,-0.7,0.2);
limits((0,-10,0),(10,10,10));
xaxis3("$x$", Arrow3);
yaxis3("$y$", Arrow3);
zaxis3("$z$", Arrow3);

var A = (1,2,3);
var B = (-3,1,-2);
var C = (2,-3,-1);

draw(box(O, A), lightolive+linewidth(0.6pt)+dashed);
draw(box(O, B), mediumgray+linewidth(0.6pt)+dashed);
draw(box(O, C), black+linewidth(0.6pt)+dashed);

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

path3 PA = plane(2*A,2*B,-1*A-1*B);
draw(PA, blue);

dot(A+B+C, blue+linewidth(5.0pt));
label("$u+v+w$",A+B+C, S, blue);

dot(2*A+2*B+C, blue+linewidth(5.0pt));
label("$2u+2v+w$", 2*A+2*B+C, S, blue);