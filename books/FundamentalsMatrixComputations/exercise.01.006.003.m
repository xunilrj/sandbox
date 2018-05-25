load west0479.mat;
A = Problem.A;
issparse(A)
spy(A);
nnz(A) # for some readon is diffent from the book's version

A = A'*A;
issparse(A)
spy(A);
nnz(A)

tic, R = chol(A); toc
nnz(R)

# Random Permutation

p = randperm(479) ; 
arnd = A (p, p) ; 
spy(arnd) 
tic, rrnd = chol(arnd); toc
nz = nnz(rrnd) 
spy(rrnd) 

# SYMmetric Reverse Cuthill-McKee.

p = symrcm(A); 
arcm = A(p,p); 
spy(arcm) 
tic, rrcm = chol(arcm); toc 
nz = nnz(rrcm) 
spy(rrcm) 

# SYMmetric MiniMum Degree

p = symamd(A); 
asmd = A(p,p); 
spy(asmd) 
tic, rsmd = chol(asmd); toc 
nz = nnz(rsmd) 
spy(rsmd)
