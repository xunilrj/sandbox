/ \l C:\github\xunilrj-sandbox\sources\kdb\stat.tests.q
\l qunit.q

.stattests.beforeNamespaceGenerateNumbers:{
 n:.stat.u12 1000000;
 }


.stattests.testu12AverageMustBeZero:{ 
 av:avg n;
 s:sdev n;
 .qunit.assertEquals[av < 0.001; 1b; "avg of 1000000 Uniform12 must be zero"];
 .qunit.assertEquals[(s-1f) < 0.001; 1b; "sdev of 1000000 Uniform12 must be 1"];
 };

.qunit.runTests `.stattests

