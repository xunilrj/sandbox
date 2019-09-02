% number of repeats:% 3
% enter first, last, inc:% 48 1488 48 
data = [
%  n          reference      |         current implementation 
%        time       GFLOPS   |    time       GFLOPS     diff 
  1488 2.8262e-01 2.3315e+01    8.2301e-01 8.0063e+00 1.8190e-12
  1440 2.6538e-01 2.2503e+01    9.8540e-01 6.0605e+00 1.8190e-12
  1392 2.3251e-01 2.3201e+01    7.5099e-01 7.1831e+00 1.6485e-12
  1344 2.0830e-01 2.3309e+01    8.5054e-01 5.7087e+00 1.5348e-12
  1296 2.1074e-01 2.0658e+01    5.9743e-01 7.2871e+00 1.3642e-12
  1248 1.6712e-01 2.3262e+01    6.0614e-01 6.4136e+00 1.4211e-12
  1200 1.4790e-01 2.3368e+01    4.4064e-01 7.8431e+00 1.1369e-12
  1152 1.3128e-01 2.3291e+01    4.7664e-01 6.4150e+00 9.6634e-13
  1104 1.1773e-01 2.2858e+01    3.3688e-01 7.9884e+00 6.8212e-13
  1056 1.0422e-01 2.2598e+01    3.2534e-01 7.2390e+00 5.1159e-13
  1008 8.8737e-02 2.3084e+01    2.6547e-01 7.7161e+00 2.2737e-13
   960 7.5989e-02 2.3286e+01    2.3239e-01 7.6143e+00 1.7053e-13
   912 6.5723e-02 2.3083e+01    1.4852e-01 1.0215e+01 1.1369e-13
   864 5.7424e-02 2.2464e+01    9.0653e-02 1.4229e+01 1.1369e-13
   816 4.7085e-02 2.3079e+01    7.2524e-02 1.4984e+01 1.4211e-13
   768 3.8966e-02 2.3250e+01    8.6812e-02 1.0436e+01 1.4211e-13
   720 3.2898e-02 2.2691e+01    4.8068e-02 1.5530e+01 1.1369e-13
   672 2.6720e-02 2.2714e+01    3.9143e-02 1.5505e+01 1.1369e-13
   624 2.1482e-02 2.2621e+01    3.1123e-02 1.5614e+01 1.1369e-13
   576 1.6901e-02 2.2614e+01    2.5607e-02 1.4926e+01 1.7053e-13
   528 1.3799e-02 2.1335e+01    1.9195e-02 1.5337e+01 1.1369e-13
   480 1.0156e-02 2.1779e+01    1.4665e-02 1.5082e+01 1.1369e-13
   432 7.4390e-03 2.1675e+01    1.0282e-02 1.5682e+01 5.6843e-14
   384 4.9270e-03 2.2985e+01    8.7580e-03 1.2931e+01 5.6843e-14
   336 3.4620e-03 2.1914e+01    4.8320e-03 1.5701e+01 5.6843e-14
   288 2.1780e-03 2.1936e+01    2.9740e-03 1.6064e+01 7.1054e-14
   240 1.1940e-03 2.3156e+01    1.7050e-03 1.6216e+01 4.2633e-14
   192 6.2300e-04 2.2722e+01    8.6800e-04 1.6308e+01 2.8422e-14
   144 2.7000e-04 2.2118e+01    3.2800e-04 1.8207e+01 2.8422e-14
    96 8.7000e-05 2.0339e+01    9.9000e-05 1.7873e+01 1.0658e-14
    48 1.6000e-05 1.3824e+01    1.4000e-05 1.5799e+01 5.3291e-15
];

% Maximum difference between reference and your implementation: 1.818989e-12.