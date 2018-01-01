function b = mv_mult(n, A,x)
    for j = 1:n 
        for i = 1:n
            b(i) = A(i,j) * x(j);
        end 
    end
endfunction

n = 1; 
for jay = 1:16
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
            time
            ratio = time/oldtime
        else
            ratio = 0
        end
    end 
    n = 2*n; 
end

n = 1; 
for jay = 1:20
    n
    if jay > 1 
        oldtime = time; 
    end 
    A = randn(n); 
    x = randn(n,1); 
    t = cputime; 
    b = mv_mult(n, A, x); 
    matrixsize = n; 
    time = cputime - t;
    if jay > 1 
        if(oldtime > 0)
            time
            ratio = time/oldtime
        else
            ratio = 0
        end
    end 
    n = 2*n; 
end