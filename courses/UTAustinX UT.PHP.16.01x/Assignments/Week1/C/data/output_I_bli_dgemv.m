% number of repeats:% 3
% enter first, last, inc:% 48 480 48 
data = [
%  n          reference      |         current implementation 
%        time       GFLOPS   |    time       GFLOPS     diff 
   480 9.7720e-03 2.2634e+01    3.5370e-02 6.2534e+00 8.5265e-14
   432 7.0990e-03 2.2713e+01    2.5585e-02 6.3023e+00 5.6843e-14
   384 5.0030e-03 2.2636e+01    1.8156e-02 6.2374e+00 5.6843e-14
   336 3.3860e-03 2.2406e+01    1.2129e-02 6.2549e+00 5.6843e-14
   288 2.0760e-03 2.3013e+01    7.2480e-03 6.5916e+00 4.2633e-14
   240 2.7730e-03 9.9704e+00    4.3360e-03 6.3764e+00 3.5527e-14
   192 6.2200e-04 2.2758e+01    2.2100e-03 6.4053e+00 2.8422e-14
   144 2.7100e-04 2.2037e+01    8.8400e-04 6.7556e+00 2.8422e-14
    96 2.1900e-04 8.0798e+00    7.0200e-04 2.5206e+00 1.0658e-14
    48 4.1000e-05 5.3947e+00    9.2000e-05 2.4042e+00 5.3291e-15
];

% Maximum difference between reference and your implementation: 8.526513e-14.