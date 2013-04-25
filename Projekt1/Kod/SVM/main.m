%% Setup, data.

load('../data_2000.mat');

CVP = cvpartition(size(wordcount,2), 'k', 10);
nWords = max(cellfun(@(x) max(x.id), wordcount))
% kernels = {'polynomial', 'quadratic', 'linear', 'rbf'};
kernels = {'rbf'};
for k = 1:3

%for i = 1:CVP.NumTestSets
for i = 1:1
    % Training data
    training_data = wordcount(CVP.training(i));

    %training_data = training_data(1:4500);
    training_targets = labels_sentiment(CVP.training(i));
    %training_targets = training_targets(1:4500);
    test_data = wordcount(CVP.test(i));
    test_targets = labels_sentiment(CVP.test(i));
   tic; 
	disp(kernels{k})
    classifications = run_svm( training_data, training_targets, test_data, nWords, kernels{k});
   toc; 
    correctFraction = sum(classifications == test_targets) / length(test_targets);
    
    disp(sprintf('Classified correctly: %1.2f percent.', correctFraction));
end
end
