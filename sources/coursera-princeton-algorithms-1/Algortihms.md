# Karatsuba Multiplication

l = a*10^N + b  
r = c*10^N + d  

l*r = (a*10^N + b)*(c*10^N + d)  
    = (a*c*10^2N)+(a*d*10^N)+(b*c*10^N)+(b*d)  
    = (a*c*10^2N)+([(a*d)+(b*c)]*10^N)+]+(b*d)  

but (a*d)+(b*c) is equal to
    = (a+b)*(c+d) - ac - bd
    = ac + ad + bc + bd - ac - bd
    = ad + bc

not we can change two muliplications (a*d) and (b*c) to just one (a+b)*(c+d), gievn that we have already calculated a*c and b*d.  

final:  
l*r = (a*c*10^2N)+([(a+b)*(c+d) - a*c - b*d]*10^N)+]+(b*d)