import three;
import grid3;

size(8cm,0);
currentprojection=orthographic(1,-1,0.5);
limits((0,-2,0),(2,2,2));
xaxis3("$i$", Arrow3);
yaxis3("$j$", Arrow3);
zaxis3("$k$", Arrow3);

var A = (1,0,0);
var B = (0,1,0);
var C = (0,0,1);

draw(box(O, (1,1,1)), black+linewidth(0.6pt)+dashed);

dot(A);dot(Label("$i$"), A);
dot(B);dot(Label("$j$"), B);
dot(C);dot(Label("$k$"), C);

dot((1,1,1));dot(Label("$i+j+k$"), (1,1,1));

srand(0);
for(var x = 0;x <= 1000; ++x)
{
    var a = unitrand()*2;
    var b = unitrand()*2;
    var c = unitrand()*2;     
    dot(a*A+b*B+c*C, red+linewidth(1.0pt));
}
