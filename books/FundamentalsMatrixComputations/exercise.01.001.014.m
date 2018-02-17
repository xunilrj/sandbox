n = 1; 
for jay = 1:15
    n
    if jay > 1 
        oldtime = time; 
    end 
    A = randn(n); 
    X = randn(n); 
    t = cputime; 
    B = A*X; 
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