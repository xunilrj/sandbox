img = imread('cameraman.tif');

%Threshold the image at the intensity level 128
%make all the values in the image less than 128 to 0 and equal or greater than 128 to 255

imgT = img;
imgT(img >= 128) = 255;
imgT(img < 128) = 0;

figure;
subplot(1,2,1)
imshow(img)
subplot(1,2,2)
imshow(imgT)
