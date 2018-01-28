warning('off','all');


pkg load symbolic;


function result = newui(i) result = sym(["u" num2str(i)]); endfunction
function result = formula(m)
    syms x c d;
    u = arrayfun(@newui, 0:m);
    h = 1/m;
    A = zeros(m,m)*x;
    for i=2:m        
        result = (-u(i+1) + 2*u(i) - u(i-1))/(h*h);
        result += -c*((u(i+1)-u(i-1))/(2*h));
        result += d*u(i);        
        result = subs(result, u(1), 0);
        result = subs(result, u(m+1), 0);      
        for exportedi=2:m
            expr = result;
            for zerodii=2:m
                if(exportedi != zerodii)
                    expr = subs(expr, u(zerodii), 0); 
                endif
            end
            expr = subs(expr, u(exportedi), 1);
            A(exportedi,i-1) = expr;
        end        
    end
    result = A;
endfunction

formula(6)
formula(8)
formula(20)