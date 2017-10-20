function [ x_out ] = laff_axpy( alpha, x, y )
    if ~isscalar(alpha)
        x_out = 'FAILED';
        return
    end
    
    [m_x,n_x] = size(x);
    [m_y,n_y] = size(y);
    
    if (~isvector(x) || ~isvector(y))
        x_out = 'FAILED';
        return
    end    
    
    if ( m_x ~= 1 && n_x ~= 1 ) || ( m_y ~= 1 && n_y ~= 1 )
        x_out = 'FAILED';
        return
    end
    if ( m_x * n_x ~= m_y * n_y )
        x_out = 'FAILED';
        return
    end
    
    x_out = y;
    if ( n_x == 1 )     % x is a column vector
        if ( n_y == 1 )     % y is a column vector
            % Copy the elements of x into the elements of y
            for i=1:m_x   
                x_out( i,1 ) = (alpha * x( i,1 )) + y(i,1);
            end
        else     % y is a row vector
            % Copy the elements of x into the elements of y
            for i=1:m_x   
                x_out( 1,i ) = (alpha * x( i,1 )) + y(1,i);
            end
        end
    else    % x is a row vector
        if ( n_y == 1 )     % y is a column vector
            % Copy the elements of x into the elements of y
            for i=1:n_x   
                x_out( i,1 ) = (alpha * x( 1,i )) + y(i,1);
            end
        else     % y is a row vector
            % Copy the elements of x into the elements of y
            for i=1:n_x   
                x_out( 1,i ) = (alpha * x( 1,i )) + y(1,i);
            end
        end
    end

end
