

img = imread('cameraman.tif');
dimg = im2double(img);
subimg1  = img(1:50, 1:50);
subimg2  = img(207:256, 207:256);
SSD = immse(subimg1, subimg2) * numel(subimg1);