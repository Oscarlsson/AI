function [trainingData, testData] = init_trainingdata_and_testdata(training_data, test_data, nWords)

% Initialize training data
nTrainingDocuments = size(training_data, 2);
trainingData = zeros(nTrainingDocuments, nWords);
for d = 1:nTrainingDocuments
    for i = 1:size(training_data{d}.id, 2)
        w = training_data{d}.id(i);
        tfidf = training_data{d}.cnt(i);
        trainingData(d,w) = tfidf;
    end
end

% Initialize test data
nTestDocuments = size(test_data, 2);
testData = zeros(nTestDocuments, nWords);
for d = 1:nTestDocuments
    for i = 1:size(test_data{d}.id, 2)
        w = test_data{d}.id(i);
        tfidf = test_data{d}.cnt(i);
        testData(d,w) = tfidf;
    end
end

end
