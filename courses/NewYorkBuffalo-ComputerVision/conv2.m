%read the image
img = imread('Davis_Hall.jpg');

I = rgb2gray(img);
Gx = [-1 0 1; -2 0 2;-1 0 1];
Gy = [-1 -2 -1; 0 0 0;1 2 1];




Fx = myConv2(I, Gx, 'same');
Fy = myConv2(I, Gy, 'same');

FMag = sqrt(Fx.*Fx + Fy.*Fy);

%uncomment the code as needed
figure
imshow(I);
title('Original Image');
figure
imshow((abs(Fx))./max(max(Fx)))
title('Gradient in X direction');
figure
imshow(abs(Fy)./max(max(Fy)))
title('Gradient in Y direction');
figure
imshow(FMag./max(max(FMag)))
title('Gradient Magnitude');

function varargout = arr2vars(arr)
  N = numel(arr);
  if nargout ~= N
      error('Number of outputs does not match number of elements')
  end
  for k = 1:N
      varargout{k} = arr(k);
  end
end

function B = myCorr2(A, k, shape)
  [r, c] = size(A);    
  [m, n] = size(k);
  
  % Correlation result will be increased on the borders
  % We will call this paddings by 
  % https://github.com/vdumoulin/conv_arithmetic  

  center = floor((size(k)+1)/2);
  [paddingTop paddingLeft] = arr2vars(center - 1);
  [paddingBottom paddingRight] = arr2vars([m n] - center);

  % Create a new image and copy the image A to this image
  % keeping the paddings as zero
  paddedA = zeros(r + paddingTop + paddingBottom, c + paddingLeft + paddingRight);
  for x = 1 + paddingTop : r + paddingTop
    for y = 1 + paddingLeft : c + paddingLeft
      paddedA(x,y) = A(x - paddingTop, y - paddingLeft);
    end
  end

  % Create the result image as zero
  % and apply the kernel
  % see https://uk.mathworks.com/help/matlab/ref/conv2.html
  % Subsection of the convolution, specified as one of these values:
  % 'full' — Return the full 2-D convolution.
  % 'same' — Return the central part of the convolution, which is the same size as A.
  % 'valid' — Return only parts of the convolution that are computed without zero-padded edges.
  if(shape == "same")
    B = zeros(r , c);
    for x = 1 : r
      for y = 1 : c
          for i = 1 : m
              for j = 1 : n
                  q = x - 1;
                  w = y -1;
                  B(x, y) = B(x, y) + (paddedA(i + q, j + w) * k(i, j));
              end
          end
      end
    end
  else
    throw (MException('myConv2', 'shape %s not implemented', shape));
  end
end

function B = myConv2(A, k, shape)
    % why rotate? see https://www.youtube.com/watch?v=C3EEy8adxvc
    % why have two different definitions? correlations versus convolutions?
    % convolutions are associative; correlations are not.
    % (x <op> y) <op> z = x <op> (y <op> z) for all x, y, z
    k = rot90(k, 2); 
    B = myCorr2(A, k, shape);   
end