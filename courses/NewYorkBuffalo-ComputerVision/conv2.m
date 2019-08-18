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

function B = myConv2(A, k, shape)
    [r, c] = size(A);
    [m, n] = size(k);
    h = rot90(k, 2);
    center = floor((size(h)+1)/2);
    left = center(2) - 1;
    right = n - center(2);
    top = center(1) - 1;
    bottom = m - center(1);
    Rep = zeros(r + top + bottom, c + left + right);
    for x = 1 + top : r + top
      for y = 1 + left : c + left
          Rep(x,y) = A(x - top, y - left);
      end
    end
    B = zeros(r , c);
    for x = 1 : r
      for y = 1 : c
          for i = 1 : m
              for j = 1 : n
                  q = x - 1;
                  w = y -1;
                    B(x, y) = B(x, y) + (Rep(i + q, j + w) * h(i, j));
              end
          end
      end
    end
end