 x,y = var("x y")
eq = y^2-x^2
[sx,ex,sy,ey]=[-10,10,-10,10]
p  = implicit_plot(eq==0,(x,sx,ex),(y,sy,ey), color="blue")
p += implicit_plot(eq==2,(x,sx,ex),(y,sy,ey), color="red")
p += implicit_plot(eq==-2,(x,sx,ex),(y,sy,ey), color="green")
p += plot_slope_field(eq,(x,sx,ex),(y,sy,ey), headlength=1e-8)
p.show(aspect_ratio=1)
