load('../data.mat');

CVP = cvpartition(size(wordcount,2), 'k', 10);

for i = 1:CVP.NumTestSets

    training_data = wordcount(CVP.training(i));
    training_targets = labels_sentiment(CVP.training(i));
    test_data = wordcount(CVP.test(i));
    test_targets = labels_sentiment(CVP.test(i));
    
    classifications = run_average_perceptron(training_data, training_targets, test_data);
    correctFraction = sum(classifications == test_targets) / length(test_targets);
    disp(sprintf('(AP) Classified correctly: %1.2f.', correctFraction));
    
    classifications = run_perceptron(training_data, training_targets, test_data);
    correctFraction = sum(classifications == test_targets) / length(test_targets);
    disp(sprintf('(P) Classified correctly: %1.2f.', correctFraction)); 
end