%%
%
% http://www.bmva.org/bmvc/1988/avc-88-023.pdf
%
% MORAVEC REVISITED
%
% Moravec's corner detector functions by considering a local
% window in the image, and determining the average changes
% of image intensity that result from shifting the window by
% a small amount in various directions. Three cases need to
% be considered:
%
% A. If the windowed image patch is flat (ie. approximately
% constant in intensity), then all shifts will result in only
% a small change;
%
% B. If the window straddles an edge, then a shift along the
% edge will result in a small change, but a shift
% perpendicular to the edge will result in a large change;
%
% C. If the windowed patch is a corner or isolated point, then
% all shifts will result in a large change. A corner can
% thus be detected by finding when the minimum change
% produced by any of the shifts is large.
%
% We now give a mathematical specification of the above.
% Denoting the image intensities by I, the change E produced
% by a shift (x,y) is given by:
%
% E_xy = summ_uv (w_uv * (|I_x+u,y+v - I_uv|)^2 )
%
% where w specifies the image window: it is unity within a
% specified rectangular region, and zero elsewhere. The shifts,
% (x,y), that are considered comprise {(1,0), (1,1), (0,1),
% (-1,1)}. Thus Moravec's corner detector is simply this:
% look for local maxima in min{E} above some threshold
% value.

%  The Moravec operator suffers from a number of problems;
% these are % listed below, together with appropriate corrective
% measures:
%
% 1. The response is anisotropic because only a
% discrete set of shifts at every 45 degrees is
% considered

% 2. The response is noisy because the window is
% binary and rectangular - use a smooth circular
% window, for example a Gaussian:

% 3. The operator responds too readily to edges
% because only the minimum of E is taken into
% account - reformulate the corner measure to make use of
% the variation of E with the direction of shift.

img = imread('image.jpg');
sigma=3; thresh=10000000; odr=9;
k = 0.05;
dy = [-1 0 1; -1 0 1; -1 0 1];
dx = dy';
gaussian = fspecial('gaussian', max(1, fix(6*sigma)), sigma);

dimg = rgb2gray(img);
Iy = conv2(dimg, dy, 'same');
Ix = conv2(dimg, dx, 'same');

Iy2 = Iy .* Iy;
Ix2 = Ix .* Ix;
Ixy = Ix .* Iy;

gIy2 = conv2(Iy2, gaussian, 'same');
gIx2 = conv2(Ix2, gaussian, 'same');
gIxy = conv2(Ixy, gaussian, 'same');

detM = (gIx2 .* gIy2) - (gIxy .* gIxy);
traceM = gIx2 + gIy2;
response = detM - k*(traceM).^2;

% Write your code above till response calculation
% Final local maxima and perform nonmaximal suppression and threshold
mx = ordfilt2(response,odr^2,ones(odr)); % Grey-scale dilate
final_response = (response==mx)&(response>thresh); % Find maxima and thresholding
[rows,columns] = find(final_response); % Find row,col coords.
figure; imshow(img);
hold on;
p=[columns rows];
plot(p(:,1),p(:,2),'ys');
% plot(p(:,1),p(:,2),'or');
title('\bf Harris Corners')
