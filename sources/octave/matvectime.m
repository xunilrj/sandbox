n = 200; 
for jay = 1:4 
    if jay > 1 
        oldtime = time; 
    end 
    A = randn(n); 
    x = randn(n,1); 
    t = cputime;
    b = A*x; 
    matrixsize = n
    time = cputime - t 
    if jay > 1 
        ratio = time/oldtime 
    end 
    n = 2*n; 
end 
