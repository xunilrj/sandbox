
% based on
% Reinhard, Erik, et al. "Color transfer between images." 
% IEEE Computer graphics and applications 21.5 (2001): 34-41.
% https://ieeexplore.ieee.org/abstract/document/946629

function outChannel = colortransfer(sourceChannel,targetChannel)
    sourceMean = mean(sourceChannel(:));
    sourceStDev = std(sourceChannel(:));
    
    targetMean = mean(targetChannel(:));    
    targetStDev = std(targetChannel(:));
    
    a = sourceChannel - sourceMean;
    b = targetStDev / sourceStDev;
    outChannel = (a * b) + targetMean;
end

img1 = imread('source.jpg');
img2 = imread('target.jpg');

[SL,SA,SB] = imsplit(rgb2lab(img1));
[T1,T2,T3] = imsplit(rgb2lab(img2));

OL = colortransfer(SL, T1);
OA = colortransfer(SA, T2);
OB = colortransfer(SB, T3);

newImg = lab2rgb(cat(3, OL, OA, OB));
montage({img1, img2, newImg}, 'Size', [1, 3])
