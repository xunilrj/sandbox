% Image Processing: Principles and Applications 
%
% 8.7 K-MEANS CLUSTERING ALGORITHM 
%
% In K-means clustering approach, we partition the set of input patterns S into 
% a set of K partitions, where K is known in advance. The method is based on 
% the identification of the centroids of each of the K clusters. Thus, instead of 
% computing the pairwise interpattern distances between all the patterns in all 
% the clusters, here the distances may be computed only from the centroids. The 
% method thus essentially boils down to searching for a best set of K centroids 
% of the clusters as follows: 
%
% Step 1: Select K initial cluster centers C1, C2, ..., Ck
% Step 2: Assign each pattern X \in S to a cluster Ci (1 <= i <= K), whose 
% centroid is nearest to pattern X. 
% Step 3: Recompute the centroids in each cluster Cj (1 <= j <= K) in which 
% there has been any addition or deletion of pattern points. 
% Step 4: Jump to step 2, until convergence is achieved.

img = imread('image.png');
centers = [
    0.2 0.1 0.2;
    0.8 0.5 0.2;
    0.6 0.2 0.1
];
K = size(centers,1);

imgOriginal = im2double(img);
Width = size(imgOriginal, 1);
Height = size(imgOriginal, 2);
QtdPixels = Width*Height;
Pixels = reshape(imgOriginal, QtdPixels, 3);

distances = zeros(QtdPixels, K);
mindistances = zeros(QtdPixels, 1);
labels = zeros(QtdPixels, 1);

iterations = 3;
for n = 1:iterations
    % Update all pixels
    for pixeli = 1:QtdPixels
        for centerj = 1:K  
            distances(pixeli,centerj) = norm(Pixels(pixeli,:) - centers(centerj,:));      
        end
        [d, l] = min(distances(pixeli, 1:K));
        mindistances(pixeli) = d;
        labels(pixeli) = l;
    end
    % Update centers positions
    for k = 1:K        
        whereCategorizedAsK = labels(:) == k;
        centers(k,:) = mean(Pixels(whereCategorizedAsK,:));       
        if sum(isnan(centers(k,:))) ~= 0            
            centers(k,:) = [0.1 0.1 0.1];            
        end
    end
end

% Each pixel in the image is the
% color of the label center
imgSegmented = zeros(size(Pixels));
for pixeli = 1:QtdPixels
    imgSegmented(pixeli,:) = centers(labels(pixeli,:),:);
end
imgSegmented = reshape(imgSegmented, Width, Height, 3);

%% Show
figure()
subplot(121); imshow(imgOriginal); title('original')
subplot(122); imshow(imgSegmented); title('segmented')
disp('number of segments ='); disp(K)
final_centers = centers
disp(centers)
