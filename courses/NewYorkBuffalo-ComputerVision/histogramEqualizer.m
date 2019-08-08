% Histogram Equalization
% https://archive.fo/QZYbN

% Image Processing: Principles and Applications
% Wiley-Interscience 2005 ISBN 0-471-71998-6

% 6.4 HISTROGRAM-BASED CONTRAST ENHANCEMENT
%
% In a poorly contrasted image a large number of pixels occupy only a small 
% portion of the available range of intensities. Through histogram modification 
% we reassign each pixel with a new intensity value so that the dynamic range 
% of gray levels is increased. The principle here is to stretch the dynamic range 
% of the pixel values in such a way that the lighter pixels may turn still lighter, 
% while the comparatively darker pixels may be still darkened. It is quite obvious 
% that by suitably stretching the pixel values the overall contrast of the image 
% will increase. 
%
% 6.4.1 Image Histogram 
%
% Histogram of an image represents the relative frequency of occurrence of the 
% various gray levels in the image. Mathematically speaking for a digital image 
% with gray levels in the range [0, L-1], the histogram is a discrete function 
% p(r_k) = n_k/N, where r_k is the k-th gray level, and n_k is the number of pixels in 
% the image with that gray level. N is the total number of pixels in the image. 
% It may be noted that k = 0, 1, 2, ..., L-1. 
%
% 6.4.2 Histogram Equalization 
%
% Histogram equalization is a technique which consists of adjusting the gray 
% scale of the image so that the gray level histogram of the input image is 
% mapped onto a uniform histogram.

img = imread('pout.tif');
[M, N] = size(img);

H = imhist(img, 256);
Hc = cumsum(H);

MN = M * N;
f = 255 / MN;
T = round(f * Hc);

hold on
    plot(H);
yyaxis right
    plot(Hc);
hold off

pixels = uint8(T(img(:)));
img_heq = reshape(pixels, M, N);
imshow(img_heq);