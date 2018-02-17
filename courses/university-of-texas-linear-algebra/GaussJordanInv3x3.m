function result = GaussJordanInv3x3( A )
    appended = [A eye(size(A))];
    G0 = [
        1 0 0
        -A(2,1)/A(1,1) 1 0
        -A(3,1)/A(1,1) 0 1
    ];
    A1 = G0*appended;
    G1 = [
        1 -A1(1,2)/A1(2,2) 0
        0 1 0
        0 -A1(3,2)/A1(2,2) 1
    ];
    A2 = G1*A1;
    G2 = [
        1 0 -A2(1,3)/A2(3,3)
        0 1 -A2(2,3)/A2(3,3)
        0 0 1
    ];
    A3 = G2*A2;
    D = [
        1/A3(1,1) 0 0
        0 1/A3(2,2) 0
        0 0 1/A3(3,3)
    ];
    A4 = D*A3;
    result = A4(:, 4:6);
endfunction
