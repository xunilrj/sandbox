function [ norm2 ] = laff_norm2( x )
    if ~isvector(x)
        norm2 = 'FAILED';
        return
    end
    [cols, rows] = size(x);
    if(cols ~= 1 && rows ~= 1)
        norm2 = 'FAILED';
        return
    end
    norm2 = sqrt(laff_dot(x,x));
end