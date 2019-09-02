% number of repeats:% 3
% enter first, last, inc:% 48 1488 48 
data = [
%  n          reference      |         current implementation 
%        time       GFLOPS   |    time       GFLOPS     diff 
  1488 2.7516e-01 2.3947e+01    5.5421e-01 1.1889e+01 1.8190e-12
  1440 2.5394e-01 2.3517e+01    5.3000e-01 1.1268e+01 1.8190e-12
  1392 2.2506e-01 2.3968e+01    4.3264e-01 1.2469e+01 1.6485e-12
  1344 2.0373e-01 2.3833e+01    4.1901e-01 1.1588e+01 1.5348e-12
  1296 1.8275e-01 2.3823e+01    3.3588e-01 1.2962e+01 1.3642e-12
  1248 1.6314e-01 2.3829e+01    3.2198e-01 1.2074e+01 1.4211e-12
  1200 1.6702e-01 2.0692e+01    2.6544e-01 1.3020e+01 1.1369e-12
  1152 1.2884e-01 2.3733e+01    2.6768e-01 1.1423e+01 9.6634e-13
  1104 1.1293e-01 2.3830e+01    1.9758e-01 1.3621e+01 6.8212e-13
  1056 1.0012e-01 2.3523e+01    1.7291e-01 1.3621e+01 5.1159e-13
  1008 8.6536e-02 2.3671e+01    2.8383e-01 7.2170e+00 2.2737e-13
   960 7.4429e-02 2.3774e+01    2.0515e-01 8.6255e+00 1.7053e-13
   912 6.5095e-02 2.3306e+01    1.3482e-01 1.1252e+01 1.1369e-13
   864 5.4780e-02 2.3548e+01    9.3408e-02 1.3810e+01 1.1369e-13
   816 4.6935e-02 2.3153e+01    6.7034e-02 1.6211e+01 1.4211e-13
   768 3.8661e-02 2.3434e+01    5.4704e-02 1.6561e+01 1.4211e-13
   720 3.2714e-02 2.2819e+01    4.2412e-02 1.7601e+01 1.1369e-13
   672 2.6878e-02 2.2581e+01    3.4981e-02 1.7350e+01 1.1369e-13
   624 2.1337e-02 2.2775e+01    2.8741e-02 1.6908e+01 1.1369e-13
   576 1.6895e-02 2.2622e+01    2.1602e-02 1.7693e+01 1.7053e-13
   528 1.3085e-02 2.2499e+01    1.6724e-02 1.7603e+01 1.1369e-13
   480 9.8940e-03 2.2355e+01    1.2457e-02 1.7756e+01 1.1369e-13
   432 7.1890e-03 2.2429e+01    8.9200e-03 1.8077e+01 5.6843e-14
   384 5.0550e-03 2.2403e+01    6.3490e-03 1.7837e+01 5.6843e-14
   336 3.3170e-03 2.2872e+01    4.1010e-03 1.8499e+01 5.6843e-14
   288 2.1140e-03 2.2600e+01    6.1930e-03 7.7145e+00 7.1054e-14
   240 2.9430e-03 9.3945e+00    1.4320e-03 1.9307e+01 4.2633e-14
   192 6.2800e-04 2.2541e+01    7.0500e-04 2.0079e+01 2.8422e-14
   144 2.7200e-04 2.1956e+01    2.9200e-04 2.0452e+01 2.8422e-14
    96 8.8000e-05 2.0108e+01    8.9000e-05 1.9882e+01 1.0658e-14
    48 1.7000e-05 1.3011e+01    1.1000e-05 2.0108e+01 5.3291e-15
];

% Maximum difference between reference and your implementation: 1.818989e-12.