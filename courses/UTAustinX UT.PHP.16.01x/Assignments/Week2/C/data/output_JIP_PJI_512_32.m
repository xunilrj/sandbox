% number of repeats:% 3
% enter first, last, inc:% 48 1488 48 
data = [
%  n          reference      |         current implementation 
%        time       GFLOPS   |    time       GFLOPS     diff 
  1488 2.8084e-01 2.3462e+01    1.2675e+00 5.1987e+00 1.8190e-12
  1440 2.5064e-01 2.3826e+01    1.1517e+00 5.1855e+00 1.8758e-12
  1392 2.2951e-01 2.3504e+01    1.0231e+00 5.2724e+00 1.5916e-12
  1344 2.0664e-01 2.3497e+01    9.2442e-01 5.2524e+00 1.5348e-12
  1296 1.8509e-01 2.3521e+01    8.1856e-01 5.3186e+00 1.3642e-12
  1248 1.6361e-01 2.3761e+01    7.4203e-01 5.2391e+00 1.4211e-12
  1200 1.4571e-01 2.3719e+01    6.6154e-01 5.2242e+00 1.1369e-12
  1152 1.3061e-01 2.3411e+01    5.8335e-01 5.2416e+00 9.6634e-13
  1104 1.1406e-01 2.3594e+01    5.0584e-01 5.3202e+00 6.8212e-13
  1056 1.0069e-01 2.3391e+01    4.5357e-01 5.1925e+00 5.1159e-13
  1008 8.7165e-02 2.3500e+01    3.6419e-01 5.6245e+00 2.2737e-13
   960 7.5832e-02 2.3334e+01    3.3665e-01 5.2561e+00 1.9895e-13
   912 6.3876e-02 2.3751e+01    2.7573e-01 5.5022e+00 1.7053e-13
   864 5.4981e-02 2.3462e+01    2.3928e-01 5.3909e+00 1.9895e-13
   816 4.6544e-02 2.3347e+01    2.0194e-01 5.3813e+00 1.4211e-13
   768 3.9700e-02 2.2820e+01    1.6910e-01 5.3576e+00 1.7053e-13
   720 3.2563e-02 2.2925e+01    1.4009e-01 5.3288e+00 1.4211e-13
   672 2.6665e-02 2.2761e+01    1.0980e-01 5.5276e+00 1.7053e-13
   624 2.1552e-02 2.2547e+01    8.6930e-02 5.5900e+00 1.4211e-13
   576 1.6832e-02 2.2707e+01    6.8431e-02 5.5853e+00 1.7053e-13
   528 1.2974e-02 2.2691e+01    5.1978e-02 5.6639e+00 1.4211e-13
   480 1.0032e-02 2.2048e+01    3.6802e-02 6.0101e+00 1.1369e-13
   432 7.2820e-03 2.2143e+01    2.6989e-02 5.9744e+00 7.1054e-14
   384 4.9130e-03 2.3050e+01    1.9232e-02 5.8884e+00 7.1054e-14
   336 3.3290e-03 2.2789e+01    1.3135e-02 5.7759e+00 5.6843e-14
   288 2.1930e-03 2.1786e+01    7.9080e-03 6.0414e+00 8.5265e-14
   240 1.2440e-03 2.2225e+01    4.6600e-03 5.9330e+00 4.2633e-14
   192 6.3000e-04 2.2469e+01    2.4500e-03 5.7779e+00 3.5527e-14
   144 2.7000e-04 2.2118e+01    1.0590e-03 5.6393e+00 2.8422e-14
    96 8.8000e-05 2.0108e+01    3.3800e-04 5.2351e+00 1.4211e-14
    48 1.6000e-05 1.3824e+01    5.1000e-05 4.3369e+00 7.1054e-15
];

% Maximum difference between reference and your implementation: 1.875833e-12.