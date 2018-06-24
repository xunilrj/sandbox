import three;
import grid3;

size(8cm,0);
currentprojection=orthographic(0.6,-0.8,0.2);
limits((0,-2,0),(2,2,2));
xaxis3("$i$", Arrow3);
yaxis3("$j$", Arrow3);
zaxis3("$k$", Arrow3);

var A = (0,0,0);
var B = (1,0,0);
var C = (0,1,0);
var D = (0,0,1);

draw(box(O, (1,1,1)), black+linewidth(0.6pt)+dashed);

dot(A);dot(Label("$A$"), A);
dot(B);dot(Label("$B$"), B);
dot(C);dot(Label("$C$"), C);
dot(D);dot(Label("$D$"), D);

var BC = B+C;
dot(BC, lightolive);dot(scale(0.4)*Label("$B+C$"), BC, lightolive);
var BCD = B+C+D;
dot(BCD, lightolive);dot(scale(0.4)*Label("$B+C+D$"), BCD, lightolive);
var CD = C+D;
dot(CD, lightolive);dot(scale(0.4)*Label("$C+D$"), CD, lightolive);
var BD = B+D;
dot(BD, lightolive);dot(scale(0.4)*Label("$B+D$"), BD, lightolive);


var C1 = (A+B+D+B+D)/4;
draw(box(O,C1), blue+dashed);dot(scale(0.3)*Label("$\frac{A+B+D+B+D}{4}$", W), C1, blue);
var C2 = (B+B+C+B+D+B+C+D)/4;
draw(box(B+C+D,C2), blue+dashed);dot(scale(0.3)*Label("$\frac{B+B+C+B+D+B+C+D}{4}$", W), C2, blue);
var C3 = (A+C+D+C+D)/4;
draw(box(O,C3), blue+dashed);dot(scale(0.3)*Label("$\frac{A+C+D+C+D}{4}$", W), C3, blue);
var C4 = (C+D+B+C+D+C+B+C)/4;
draw(box(B+C+D,C4), blue+dashed);dot(scale(0.3)*Label("$\frac{C+D+B+C+D+C+B+C}{4}$", W), C4, blue);
var C5 = (D+C+D+B+C+D+B+D)/4;
draw(box(B+C+D,C5), blue+dashed);dot(scale(0.3)*Label("$\frac{D+C+D+B+C+D+B+D}{4}$", S), C5, blue);
var C6 = (A+B+C+B+C)/4;
draw(box(O,C6), blue+dashed);dot(scale(0.3)*Label("$\frac{A+B+C+B+C}{4}$", W), C6, blue);
