function [ trainingData, trainingLabels, validationData, validationLabels ] = training_validation_partition( data, labels, validationSetSizeFraction )

nDataPoints = size(data, 1);
validationSetSize = ceil(nDataPoints / 10);
perm = randperm(nDataPoints);
validationSetIndices = perm(1:validationSetSize);
trainingSetIndices = perm(validationSetSize+1:nDataPoints);
validationData = data(validationSetIndices, :);
trainingData = data(trainingSetIndices, :);

validationLabels = labels(validationSetIndices);
trainingLabels = labels(trainingSetIndices);

end

