function [ x_out ] = laff_scal( alpha, x)
    if ~isscalar(alpha)
        x_out = 'FAILED';
        return
    end
    
    [m_x,n_x] = size(x);
    
    if (~isvector(x))
        x_out = 'FAILED';
        return
    end    
    
    x_out = x;
    if(n_x == 1)
        for i=1:m_x
            x_out(i,1) = x(i,1) * alpha;
        end
    else
        for i=1:n_x
            x_out(1,i) = x(1,i) * alpha;
        end
    end
    return
end