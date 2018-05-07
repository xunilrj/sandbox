function isPositiveDefinite(A)
    try
        chol(A);
        printf("Positive Definite\n")
    catch
        printf("Not Positive Definite\n")
    end
endfunction

A = [9,3,3;3,10,7;3,5,9];
B = [4,2,6;2,2,5;6,5,29];
C = [4,4,8;4,04,1;8,1,6];
D = [1,1,1;1,2,2;1,2,1];

isPositiveDefinite(A);
isPositiveDefinite(B);
isPositiveDefinite(C);
isPositiveDefinite(D);