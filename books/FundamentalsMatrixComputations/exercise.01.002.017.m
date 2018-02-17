warning('off','all');
pkg load symbolic

function result = newxi(i) result = sym(["u" num2str(i)]); endfunction
x = arrayfun(@newxi, 0:8);

l1 = ((x(4)-x(1))/0.1) - ((x(1)-x(2))/0.5)
l2 = ((x(1)-x(2))/0.5) - ((x(5)-x(2))/0.5) - ((x(2)-x(3))/0.2)
l3 = ((x(2)-x(3))/0.2) - ((x(6)-x(3))/0.1)
l4 = ((x(7)-x(4))/0.2) - ((x(4)-x(5))/0.1) - ((x(4)-x(1))/0.1)
l5 = ((x(4)-x(5))/0.1) + ((x(5)-x(2))/0.5) - ((x(8)-x(5))/0.1) - ((x(5)-x(6))/0.5)
l6 = ((x(5)-x(6))/0.5) + ((x(6)-x(3))/0.1) - ((x(9)-x(6))/0.2)
l7 = ((x(8)-x(7))/0.2) - ((x(7)-x(4))/0.2)

function result = Add(A,x,m,i,Eq)
    for ic = 1:m
        item = Eq;
        for ic2 = 1:m
            if(ic!=ic2)
                item = subs(item,x(ic2),0);
            endif
        end
        A(i,ic) = subs(item,x(ic),1);
    end
    result = A;
endfunction

A = zeros(9,9) * x(9);
A = Add(A,x,9,1,l1);
A = Add(A,x,9,2,l2);
A = Add(A,x,9,3,l3);
A = Add(A,x,9,4,l4);
A = Add(A,x,9,5,l5);
A = Add(A,x,9,6,l6);
A = Add(A,x,9,7,l7);
A = Add(A,x,9,8,x(8));
A = Add(A,x,9,9,x(9));
A
b = [0,0,0,0,0,0,0,9,0]'
A \ b