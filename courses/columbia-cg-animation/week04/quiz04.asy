import graph;
import geometry;
size(600,600);

point A = (-1,0);
point B = (1,1);
point C = (0,2);

xaxis("$x$",LeftTicks,Arrow);
yaxis("$y$",RightTicks,Arrow);

draw(A -- B); 
dot(C,UnFill);

path AB = A -- B;
path CA = C -- A;
path BA = B -- A;

real lBA = length(B-A);
real lCA = length(C-A);
real dotCABA = dot(C-A,B-A);

real alpha = dotCABA / (lBA*lBA);
pair PALPHA = A+(B-A)*alpha;

draw(perpendicular(C, line(A,B)), green);
draw(A--C,red,Arrow);
draw(A -- PALPHA,blue,Arrow);
draw(C -- PALPHA,red,Arrow);
dot("",PALPHA,SE);
dot("A",A,S);
dot("B",B,E);
dot("C",C,NE);

