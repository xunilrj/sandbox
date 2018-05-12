n = 1000; 
for jay = 1:4 
    if jay > 1; oldtime = time; end;
    M = randn(n); 
    A = M'*M; 
    t = cputime; 
    R = chol(A); 
    matrixsize = n
    time = cputime - t
    if time == 0; time = 1; end;
    if jay > 1; ratio = time/oldtime; end;
    n = 2*n; 
end 
