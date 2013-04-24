%% This file runs the different algos on some datasets...

% Init data and paths.
load data.mat
addpath('Percetron')
addpath('Naive Bayes')
addpath('SVM')
addpath('K Nearest')


%CV-Stuff
CVP = cvpartition(size(wordcount,2), 'k', 10);

% Training data
training_data = wordcount(CVP.training(1));
training_targets = labels_sentiment(CVP.training(1));
test_data = wordcount(CVP.test(1));
test_targets = labels_sentiment(CVP.test(1));
% Only use a subset of training/test data to decrease runtime...
test_data = test_data(1:100);
test_targets = test_targets(1:100);

% ALG

% PERCEPTRON
% Hardcoded sentiment = 1 N = 10
disp('PERCEPTRON');
tic;
classifications = run_perceptron(training_data, training_targets, test_data, 1, 10);
nErrors = sum(classifications ~= test_targets)
t = toc

% NAIVE BAYES
% Hardcoded nWords = 2000
disp('NAIVE BAYES');
tic;
classifications = run_naivebayes(training_data, training_targets, test_data, 2000);
nErrors = sum(classifications ~= test_targets)
t = toc

% K NEAREST NEIG
% Hardcoded nWords = 2000, k = 5
disp('K NEAREST');
tic;
classifications = run_knn(training_data, training_targets, test_data, 2000, 5);
nErrors = sum(classifications ~= test_targets)
t = toc

% SVM
% Todo
