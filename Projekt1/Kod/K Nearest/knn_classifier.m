function [ classification ] = knn_classifier( data, labels, K, x )

[~, sortedIndices] = ...
    sort(arrayfun(@(i) norm(data(i,:) - x), 1:size(data,1))', 1, 'ascend');

nLabels = max(labels);

classScore = zeros(1, nLabels);

for i = 1:K
    % i:th nearest neighbour
    pointIndex = sortedIndices(i);
    pointClass = labels(pointIndex);
    classScore(pointClass) = classScore(pointClass) + 1;
end

maxIx = find(classScore == max(classScore));

% maxIx = list of ties indices
if size(maxIx, 2) > 1
    bestClassIndex = randsample(maxIx, 1);
else
    bestClassIndex = maxIx;
end

classification = bestClassIndex; % Same labels as class indices

end

 