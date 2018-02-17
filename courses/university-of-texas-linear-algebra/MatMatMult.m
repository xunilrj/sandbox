function [ C_out ] = MatMatMult( A, B, C ) 
[ m, n ] = size( C );
[ m_A, k ] = size( A );
[ m_B, n_B ] = size( B );
 
for p = 1:k 
    for j = 1:n
   for i = 1:m 
       
      
          
         C( i,j ) = A( i, p ) * B( p, j ) + C( i, j );
      end 
   end 
end 
C_out = C;
