% https://www.tutorialspoint.com/dip/gray_level_transformations

img = imread('pout.tif');
img_neg = 255 - img;

imshow(img)
imshow(img_neg)
