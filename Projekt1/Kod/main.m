%% This file runs the different algos on some datasets...

% Init data and paths.
load data.mat
addpath('Percetron')
addpath('Naive Bayes')
addpath('SVM')
addpath('K Nearest')


labels = {labels_classes, labels_sentiment};

%LOOP OVER LABELS 
%for j = 1:size(labels,2)

%CV-Stuff
CVP = cvpartition(size(wordcount,2), 'k', 10);

nrAlg = 5;
timeRun = zeros(nrAlg, CVP.NumTestSets);

% LOOOOOP OVER CV 

for i = 1:CVP.NumTestSets

    % Training data
    training_data = wordcount(CVP.training(i));
    training_targets = labels_sentiment(CVP.training(i));
    test_data = wordcount(CVP.test(i));
    test_targets = labels_sentiment(CVP.test(i));
    % Only use a subset of training/test data to decrease runtime...
    test_data = test_data(1:100);
    test_targets = test_targets(1:100);

    % PERCEPTRON
    tic;
    N = 10;
    perceptron_classifications = run_perceptron(training_data, training_targets, test_data, N);
    %nErrors = sum(classifications ~= test_targets)
    correctFraction_sent_aver = sum(perceptron_classifications == test_targets) / length(test_targets);
    disp(sprintf('(P) Classified correctly: %1.2f.', correctFraction_sent_aver));
    timeRun(1,i) = toc;

    %Averaged Perceptron
    tic;
    classifications = run_average_perceptron(training_data, training_targets, test_data, N);
    correctFraction_sent_aver = sum(classifications == test_targets) / length(test_targets);
    disp(sprintf('(AP) Classified correctly: %1.2f.', correctFraction_sent_aver));
    timeRun(2,i) = toc;

    % NAIVE BAYES
    tic;
    nWords = 2000;
    classifications = run_naivebayes(training_data, training_targets, test_data, nWords);
    correctFraction_sent_aver = sum(classifications == test_targets) / length(test_targets);
    disp(sprintf('(NB) Classified correctly: %1.2f percent.', correctFraction_sent_aver));
    timeRun(3,i) = toc;

    % K NEAREST NEIG
    %tic;
    %k = 5;
    %classifications = run_knn(training_data, training_targets, test_data, nWords, k);
    %correctFraction = sum(classifications == test_targets) / length(test_targets);
    %disp(sprintf('(KNN) Classified correctly: %1.2f.', correctFraction));
    %timeRun(4,i) = toc;

    % SVM
    %tic;
    %classifications = run_svm( training_data, training_targets, test_data, nWords );
    %correctFraction = sum(classifications == test_targets) / length(test_targets);
    %disp(sprintf('(SVM) Classified correctly: %1.2f percent.', correctFraction));
    %timeRun(5,i) = toc
end