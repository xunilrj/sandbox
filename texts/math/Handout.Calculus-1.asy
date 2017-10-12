if(!settings.multipleView) settings.batchView=false;
settings.tex="pdflatex";
settings.inlinetex=true;
deletepreamble();
defaultfilename="Handout.Calculus-1";
if(settings.render < 0) settings.render=4;
settings.outformat="";
settings.inlineimage=true;
settings.embed=true;
settings.toolbar=false;
viewportmargin=(2,2);

import geometry;
import math;
import patterns;

add("hner",hatch(3mm, 0.2pt+red));
add("hnwg",hatch(3mm, NW, 0.2pt+green));

//show(defaultcoordsys);

real radius = 1;
real theta = 3.14159 / 4.0;
point A = (0,0);
point C = (radius,0);
point E = (cos(theta), sin(theta));
point D = (radius, tan(theta));

circle canonicalCircle = circle(A,radius);
triangle ACE = triangle(A,C,E);
triangle ACD = triangle(A,C,D);


draw(canonicalCircle,0.5mm+white);

line AC = line(A,C);
line AE = line(A,E);
line EC = line(E,C);
line AD = line(A,D);
line CD = line(C,D);

draw(ACE, 2pt+red);
fill(ACE.Path(), pattern("hner"));

draw(ACD, 2pt+green);
fill(ACD.Path(), pattern("hnwg"));

draw("", (segment)AC, 2pt+blue);
draw(arc(canonicalCircle,C,E),2pt+blue);
draw("", (segment)AE, 2pt+blue);

arc arctheta = arc(canonicalCircle, 0, 45);
markarc(format("θ",degrees(arctheta)), radius=10mm, arctheta, white, Arrow);

