A1 = [1;2;-1];
A2 = [3;1;0];
A3 = [2;1;1];
A = [A1,A2,A3]

x1 = [1];
x2 = [2];
x3 = [-1];
x = [x1;x2;x3]

b1 = A1*x1+A2*x2+A3*x3; 
b2 = A1*0+A2*1+A3*2;
b3 = A1*1+A2*1+A3*0;
b = [b1,b2,b3]