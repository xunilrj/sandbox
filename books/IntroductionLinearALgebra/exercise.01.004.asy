size(78cm,20cm);
import geometry;

show(defaultcoordsys);

var v=(2,1);
var w=(1,2);

draw("$\vec{v}$", origin()--origin()+v, Arrow(3mm));
draw("$\vec{w}$", origin()--origin()+w, Arrow(3mm));

srand(0);
for(var x = 0;x <= 1000; ++x)
{
    var a = unitrand()*5;
    var b = unitrand()*2;    
    dot(a*v+b*w, red+linewidth(1.0pt));
}

dot(3*v+1*w, red+linewidth(5.0pt));
label("$(3*2+1,3*1+2)=(7,5)$",3*v+1*w,S); 