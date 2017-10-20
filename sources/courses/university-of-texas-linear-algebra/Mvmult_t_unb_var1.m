
% Copyright 2017 The University of Texas at Austin
%
% For licensing information see
%                http://www.cs.utexas.edu/users/flame/license.html 
%                                                                                 
% Programmed by: Name of author
%                Email of author

function [ c_out ] = Mvmult_t_unb_var1( A, b, c )

  [ AL, AR ] = FLA_Part_1x2( A, ...
                               0, 'FLA_LEFT' );

  [ cT, ...
    cB ] = FLA_Part_2x1( c, ...
                         0, 'FLA_TOP' );

  while ( size( AL, 2 ) < size( A, 2 ) )

    [ A0, a1, A2 ]= FLA_Repart_1x2_to_1x3( AL, AR, ...
                                         1, 'FLA_RIGHT' );

    [ c0, ...
      gamma1, ...
      c2 ] = FLA_Repart_2x1_to_3x1( cT, ...
                                    cB, ...
                                    1, 'FLA_BOTTOM' );

    %------------------------------------------------------------%
    
    a1t = a1.';
    gamma1 = gamma1 + laff_dot(a1t,b);

    %------------------------------------------------------------%

    [ AL, AR ] = FLA_Cont_with_1x3_to_1x2( A0, a1, A2, ...
                                           'FLA_LEFT' );

    [ cT, ...
      cB ] = FLA_Cont_with_3x1_to_2x1( c0, ...
                                       gamma1, ...
                                       c2, ...
                                       'FLA_TOP' );

  end

  c_out = [ cT
            cB ];

return
