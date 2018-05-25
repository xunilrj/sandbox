# Wathen matrix from Higham's gallery. 

A = gallery ('wathen', 20, 15);
tic, R = chol(A); toc
nnz(A)

# SYMmetric Reverse Cuthill-McKee.
p = symrcm(A); 
arcm = A(p,p); 
tic, R = chol(arcm); toc 
nnz(R)

# SYMmetric MiniMum Degree

p = symamd(A); 
asmd = A(p,p); 
tic, rsmd = chol(asmd); toc 
nz = nnz(rsmd) 
