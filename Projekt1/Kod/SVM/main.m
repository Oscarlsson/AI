%% Setup, data.

load('../data.mat');

CVP = cvpartition(size(wordcount,2), 'k', 10);

for i = 1:CVP.NumTestSets
    % Training data
    training_data = wordcount(CVP.training(i));
    training_data = training_data(1:1000);
    training_targets = labels_sentiment(CVP.training(i));
    training_targets = training_targets(1:1000);
    test_data = wordcount(CVP.test(i));
    test_targets = labels_sentiment(CVP.test(i));
    
    classifications = run_svm( training_data, training_targets, test_data, 2000 );
    
    size(test_data)
    size(test_targets)
    size(classifications)
    
    correctFraction = sum(classifications == test_targets) / length(test_targets);
    
    disp(sprintf('Classified correctly: %1.2f percent.', correctFraction));
end