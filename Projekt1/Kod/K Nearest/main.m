% main.m consists of test code and is not necessary for any project results

clear;
load ('../data.mat');

kmax = 100;
error_rate = zeros(kmax,1);
CVP = cvpartition(size(wordcount,2), 'k', 10);

%plot error rate for different k
for K = 1:kmax
    K
    for i = 1:CVP.NumTestSets
        % Training data
        training_data = wordcount(CVP.training(i));
        training_targets = labels_sentiment(CVP.training(i));

        test_data = wordcount(CVP.test(i));
        test_targets = labels_sentiment(CVP.test(i));

        % Only use a subset of training/test data to decrease runtime...
        %training_data = training_data(1:1000);
        %training_targets = training_targets(1:1000);
        test_data = test_data(1:100);
        test_targets = test_targets(1:100);
        
        classifications = run_knn( training_data, training_targets, test_data, 2000, K);
        correctFraction = sum(classifications == test_targets) / length(test_targets);
        disp(sprintf('Classified correctly: %1.2f.', correctFraction));
        
        error_rate(K) = error_rate(K) + correctFraction;
        
    end
    error_rate(K) = error_rate(K)/CVP.NumTestSets;
    
end
x = 1:jmax;
plot(x, error_rate)
