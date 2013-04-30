function [ classifications ] = run_knn( training_data, training_labels, test_data, nWords, K)

% Initialize training data
nTrainingDocuments = size(training_data, 2);
Xtraining = zeros(nTrainingDocuments, nWords);
for d = 1:nTrainingDocuments
    for i = 1:size(training_data{d}.id, 2)
        w = training_data{d}.id(i);
        tfidf = training_data{d}.cnt(i);
        Xtraining(d,w) = tfidf;
    end
end
Ytraining = training_labels';

% Initialize test data
nTestDocuments = size(test_data, 2);
Xtest = zeros(nTestDocuments, nWords);
for d = 1:nTestDocuments
    for i = 1:size(test_data{d}.id, 2)
        w = test_data{d}.id(i);
        tfidf = test_data{d}.cnt(i);
        Xtest(d,w) = tfidf;
    end
end

classifications = zeros(1, size(test_data,2));
for i = 1:size(test_data,2)
    classifications(i) = knn_classifier(Xtraining, training_labels, K, Xtest(i,:));
end

end

