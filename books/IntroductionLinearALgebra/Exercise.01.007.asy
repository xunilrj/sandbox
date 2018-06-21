size(78cm,20cm);
import geometry;

show(defaultcoordsys);

var v=(2,1);
var w=(0,1);

draw("$\vec{v}$", origin()--origin()+v, Arrow(3mm));
draw("$\vec{w}$", origin()--origin()+w, Arrow(3mm));

for(var x = 0;x <= 2; ++x)
{
    for(var y = 0;y <= 2; ++y)
    {
        dot(x*v+y*w, red+linewidth(5.0pt));
        label("",x*v+y*w,S); 
    }
}

