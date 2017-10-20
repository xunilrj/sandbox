function [ value ] = laff_dot( x, y )
    
    [m_x,n_x] = size(x);
    [m_y,n_y] = size(y);
    
    if (~isvector(x) || ~isvector(y))
        value = 'FAILED';
        return
    end    
    
    if ( m_x ~= 1 && n_x ~= 1 ) || ( m_y ~= 1 && n_y ~= 1 )
        value = 'FAILED';
        return
    end
    if ( m_x * n_x ~= m_y * n_y )
        value = 'FAILED';
        return
    end
    
    value = 0;
    if ( n_x == 1 )     % x is a column vector
        if ( n_y == 1 )     % y is a column vector
            % Copy the elements of x into the elements of y
            for i=1:m_x   
                value = value + x( i,1 ) * y(i,1);
            end
        else     % y is a row vector
            % Copy the elements of x into the elements of y
            for i=1:m_x   
                value = value + x( i,1 ) * y(1,i);
            end
        end
    else    % x is a row vector
        if ( n_y == 1 )     % y is a column vector
            % Copy the elements of x into the elements of y
            for i=1:n_x   
                value = value + x( 1,i ) * y(i,1);
            end
        else     % y is a row vector
            % Copy the elements of x into the elements of y
            for i=1:n_x   
                value = value + x( 1,i ) * y(1,i);
            end
        end
    end

end
