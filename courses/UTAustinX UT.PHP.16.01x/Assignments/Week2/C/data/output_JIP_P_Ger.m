% number of repeats:% 3
% enter first, last, inc:% 48 480 48 
data = [
%  n          reference      |         current implementation 
%        time       GFLOPS   |    time       GFLOPS     diff 
   480 9.7380e-03 2.2713e+01    1.5993e-01 1.3830e+00 1.1369e-13
   432 7.2350e-03 2.2287e+01    1.0731e-01 1.5026e+00 7.1054e-14
   384 5.7230e-03 1.9788e+01    7.6928e-02 1.4721e+00 7.1054e-14
   336 3.2360e-03 2.3444e+01    5.0597e-02 1.4994e+00 7.1054e-14
   288 2.0530e-03 2.3271e+01    3.1830e-02 1.5010e+00 5.6843e-14
   240 1.1910e-03 2.3214e+01    1.8096e-02 1.5279e+00 4.2633e-14
   192 6.2100e-04 2.2795e+01    9.1420e-03 1.5484e+00 2.8422e-14
   144 2.7000e-04 2.2118e+01    3.9330e-03 1.5184e+00 2.8422e-14
    96 2.1900e-04 8.0798e+00    2.8420e-03 6.2262e-01 1.4211e-14
    48 4.1000e-05 5.3947e+00    3.4400e-04 6.4298e-01 7.1054e-15
];

% Maximum difference between reference and your implementation: 1.136868e-13.
