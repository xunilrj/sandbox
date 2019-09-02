% number of repeats:% 3
% enter first, last, inc:% 48 1488 48 
data = [
%  n          reference      |         current implementation 
%        time       GFLOPS   |    time       GFLOPS     diff 
  1488 3.0855e-01 2.1356e+01    2.6763e+00 2.4621e+00 1.8190e-12
  1440 2.7141e-01 2.2003e+01    2.7589e+00 2.1646e+00 1.8190e-12
  1392 2.5534e-01 2.1126e+01    2.2270e+00 2.4223e+00 1.6485e-12
  1344 2.2473e-01 2.1606e+01    2.2105e+00 2.1966e+00 1.5348e-12
  1296 2.0067e-01 2.1695e+01    1.7417e+00 2.4995e+00 1.3642e-12
  1248 1.7961e-01 2.1644e+01    1.7514e+00 2.2197e+00 1.4211e-12
  1200 1.5779e-01 2.1903e+01    1.3620e+00 2.5375e+00 1.1369e-12
  1152 1.4142e-01 2.1621e+01    1.3892e+00 2.2009e+00 9.6634e-13
  1104 1.2938e-01 2.0800e+01    1.0037e+00 2.6812e+00 6.8212e-13
  1056 1.0899e-01 2.1609e+01    9.1234e-01 2.5815e+00 5.1159e-13
  1008 9.6670e-02 2.1189e+01    6.0841e-01 3.3668e+00 2.2737e-13
   960 8.2157e-02 2.1538e+01    4.6048e-01 3.8427e+00 1.7053e-13
   912 7.4076e-02 2.0480e+01    3.4746e-01 4.3662e+00 1.1369e-13
   864 5.9965e-02 2.1512e+01    2.7592e-01 4.6750e+00 1.1369e-13
   816 5.1059e-02 2.1283e+01    2.1935e-01 4.9541e+00 1.4211e-13
   768 4.1544e-02 2.1807e+01    2.3579e-01 3.8423e+00 1.4211e-13
   720 3.5687e-02 2.0918e+01    1.4644e-01 5.0978e+00 1.1369e-13
   672 2.9790e-02 2.0374e+01    1.1928e-01 5.0883e+00 1.1369e-13
   624 2.2850e-02 2.1267e+01    9.5310e-02 5.0985e+00 1.1369e-13
   576 1.8038e-02 2.1189e+01    7.5085e-02 5.0903e+00 1.7053e-13
   528 1.3689e-02 2.1506e+01    5.7113e-02 5.1546e+00 1.1369e-13
   480 1.0486e-02 2.1093e+01    4.3014e-02 5.1421e+00 1.1369e-13
   432 7.5060e-03 2.1482e+01    3.1113e-02 5.1825e+00 5.6843e-14
   384 5.5000e-03 2.0590e+01    2.2139e-02 5.1152e+00 5.6843e-14
   336 3.7960e-03 1.9986e+01    1.4668e-02 5.1722e+00 5.6843e-14
   288 2.4630e-03 1.9397e+01    9.3120e-03 5.1306e+00 7.1054e-14
   240 1.2790e-03 2.1617e+01    5.2400e-03 5.2763e+00 4.2633e-14
   192 6.5300e-04 2.1678e+01    2.6670e-03 5.3078e+00 2.8422e-14
   144 2.7300e-04 2.1875e+01    1.0820e-03 5.5194e+00 2.8422e-14
    96 8.8000e-05 2.0108e+01    2.9900e-04 5.9180e+00 1.0658e-14
    48 1.6000e-05 1.3824e+01    3.0000e-05 7.3728e+00 5.3291e-15
];

% Maximum difference between reference and your implementation: 1.818989e-12.