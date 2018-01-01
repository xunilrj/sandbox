n = 1; 
for jay = 1:20
    n
    if jay > 1 
        oldtime = time; 
    end 
    A = randn(n); 
    x = randn(n,1); 
    t = cputime; 
    b = A*x; 
    matrixsize = n; 
    time = cputime - t;
    if jay > 1 
        if(oldtime > 0)
            ratio = time/oldtime
        else
            ratio = 0
        end
    end 
    n = 2*n; 
end