% number of repeats:% 3
% enter first, last, inc:% 48 1488 48 
data = [
%  n          reference      |         current implementation 
%        time       GFLOPS   |    time       GFLOPS     diff 
  1488 2.7707e-01 2.3782e+01    1.3112e+00 5.0255e+00 1.8190e-12
  1440 2.4946e-01 2.3940e+01    1.1639e+00 5.1309e+00 1.8758e-12
  1392 2.3819e-01 2.2647e+01    1.0545e+00 5.1156e+00 1.5916e-12
  1344 2.0445e-01 2.3749e+01    9.4844e-01 5.1194e+00 1.5348e-12
  1296 1.8276e-01 2.3821e+01    8.5627e-01 5.0844e+00 1.3642e-12
  1248 1.6363e-01 2.3758e+01    7.5655e-01 5.1385e+00 1.4211e-12
  1200 1.4473e-01 2.3880e+01    6.8701e-01 5.0305e+00 1.1369e-12
  1152 1.2955e-01 2.3603e+01    6.0537e-01 5.0509e+00 9.6634e-13
  1104 1.1432e-01 2.3540e+01    5.2733e-01 5.1034e+00 6.8212e-13
  1056 1.0026e-01 2.3490e+01    4.6108e-01 5.1080e+00 5.1159e-13
  1008 8.6565e-02 2.3663e+01    3.9887e-01 5.1354e+00 2.2737e-13
   960 7.5350e-02 2.3483e+01    3.4534e-01 5.1238e+00 1.9895e-13
   912 6.4125e-02 2.3658e+01    2.8369e-01 5.3478e+00 1.7053e-13
   864 5.4994e-02 2.3456e+01    2.4733e-01 5.2154e+00 1.9895e-13
   816 4.6470e-02 2.3384e+01    2.0841e-01 5.2142e+00 1.4211e-13
   768 3.8760e-02 2.3374e+01    1.8051e-01 5.0189e+00 1.7053e-13
   720 3.2191e-02 2.3190e+01    1.4033e-01 5.3195e+00 1.4211e-13
   672 2.6163e-02 2.3198e+01    1.1472e-01 5.2905e+00 1.7053e-13
   624 2.0978e-02 2.3164e+01    9.0108e-02 5.3929e+00 1.4211e-13
   576 1.6610e-02 2.3011e+01    7.3018e-02 5.2344e+00 1.7053e-13
   528 1.2876e-02 2.2864e+01    5.8690e-02 5.0161e+00 1.4211e-13
   480 9.7830e-03 2.2609e+01    4.1033e-02 5.3904e+00 1.1369e-13
   432 7.2890e-03 2.2121e+01    2.9935e-02 5.3864e+00 7.1054e-14
   384 5.0080e-03 2.2613e+01    2.1143e-02 5.3562e+00 7.1054e-14
   336 3.4730e-03 2.1845e+01    1.3739e-02 5.5220e+00 5.6843e-14
   288 2.4570e-03 1.9445e+01    8.8180e-03 5.4180e+00 8.5265e-14
   240 1.1970e-03 2.3098e+01    4.5880e-03 6.0262e+00 4.2633e-14
   192 6.2200e-04 2.2758e+01    2.4010e-03 5.8958e+00 3.5527e-14
   144 2.7000e-04 2.2118e+01    1.0530e-03 5.6714e+00 2.8422e-14
    96 8.8000e-05 2.0108e+01    3.3800e-04 5.2351e+00 1.4211e-14
    48 1.6000e-05 1.3824e+01    5.1000e-05 4.3369e+00 7.1054e-15
];

% Maximum difference between reference and your implementation: 1.875833e-12.