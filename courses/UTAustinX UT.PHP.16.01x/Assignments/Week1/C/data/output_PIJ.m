% number of repeats:% 3
% enter first, last, inc:% 48 480 48 
data = [
%  n          reference      |         current implementation 
%        time       GFLOPS   |    time       GFLOPS     diff 
   480 9.6240e-03 2.2983e+01    1.4574e-01 1.5176e+00 1.1369e-13
   432 7.2840e-03 2.2137e+01    1.0505e-01 1.5350e+00 7.1054e-14
   384 4.9220e-03 2.3008e+01    9.5660e-02 1.1838e+00 7.1054e-14
   336 3.3350e-03 2.2748e+01    4.9813e-02 1.5230e+00 7.1054e-14
   288 2.0640e-03 2.3147e+01    3.1174e-02 1.5326e+00 5.6843e-14
   240 1.2060e-03 2.2925e+01    1.4561e-02 1.8988e+00 4.2633e-14
   192 6.2500e-04 2.2649e+01    9.0700e-03 1.5607e+00 2.8422e-14
   144 2.7200e-04 2.1956e+01    2.8190e-03 2.1185e+00 2.8422e-14
    96 8.7000e-05 2.0339e+01    8.4000e-04 2.1065e+00 1.4211e-14
    48 1.6000e-05 1.3824e+01    9.7000e-05 2.2802e+00 7.1054e-15
];

% Maximum difference between reference and your implementation: 1.136868e-13.
