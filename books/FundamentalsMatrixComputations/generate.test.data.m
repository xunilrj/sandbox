arg_list = argv ();
size = str2num(arg_list{1});

n = 2; 
S = [1];
csvwrite ("data.csv", cell2mat ({}));
for jay = 1:size
    n
    S = [n];
    csvwrite ("data.csv", cell2mat ({S}), "-append");
    A = randn(n);
    csvwrite ("data.csv", cell2mat ({A}), "-append");    
    A = A'*A;
    csvwrite ("data.csv", cell2mat ({A}), "-append");    
    A = chol(A);
    csvwrite ("data.csv", cell2mat ({A}), "-append");
    n = n*2;
end