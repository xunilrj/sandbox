function [ A_out, b_out ] = Solve( A, b )
    LU = LU_unb_var5(A);
    L = tril( LU, -1 ) + eye( size( LU ) );
    U = triu( LU );
    y = Ltrsv_unb_var1(L, b)
    
    A_out = LU;
    b_out = Utrsv_unb_var1(U,y);
end