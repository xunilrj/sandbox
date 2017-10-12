size(200mm,200mm);
import geometry;
import math;
import patterns;

add("hner",hatch(3mm, 0.2pt+red));
add("hnwg",hatch(3mm, NW, 0.2pt+green));


real radius = 1;
real theta = 3.14159 / 4.0;
point A = (0,0);
point C = (radius,0);
point B = (cos(theta),0);
point E = (cos(theta), sin(theta));
point D = (radius, tan(theta));

circle unitcircle = circle(A,radius);
triangle ACE = triangle(A,C,E);
triangle ACD = triangle(A,C,D);

//draw(canonicalCircle,0.5mm+black);
draw((0,0)--(0,1));
draw(arc(unitcircle,(1,0),(0,1)),black);

line AC = line(A,C);
line AE = line(A,E);
line EC = line(E,C);
line AD = line(A,D);
line CD = line(C,D);
line BE = line(B,E);

draw((segment)BE, black);

draw(ACE, 2pt+red);
fill(ACE.Path(), pattern("hner"));

draw(ACD, 2pt+green);
fill(ACD.Path(), pattern("hnwg"));

draw("", (segment)AC, 2pt+blue);
draw(arc(unitcircle,C,E),2pt+blue);
draw("", (segment)AE, 2pt+blue);

arc arctheta = arc(unitcircle, 0, 45);
markarc(format("θ",degrees(arctheta)), radius=10mm, arctheta, white, Arrow);

dot("$A$",A, SW);
dot("$B$",B, S);
dot("$C$",C);
dot("$D$",D);
dot("$E$",E, N);