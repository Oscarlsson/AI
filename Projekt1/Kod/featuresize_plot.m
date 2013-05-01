%% This file runs the different algos on some datasets...
clear
% Init data and paths.
addpath('Percetron')
addpath('Naive Bayes')
addpath('SVM')
addpath('K Nearest')

N_PERCEPTRON_ITERATIONS = 25;
N_AVG_PERCEPTRON_ITERATIONS = 25;
KNN_K = 20;

algorithms = { ...
    @(training_data, training_labels, test_data, nWords) ...
        run_perceptron(training_data, training_labels, test_data, N_PERCEPTRON_ITERATIONS, nWords), ...
    @(training_data, training_labels, test_data, nWords) ...
        run_average_perceptron(training_data, training_labels, test_data, N_AVG_PERCEPTRON_ITERATIONS, nWords), ...
    @(training_data, training_labels, test_data, nWords) ...
        run_naivebayes(training_data, training_labels, test_data, nWords, true), ...
    @(training_data, training_labels, test_data, nWords) ...
        run_naivebayes(training_data, training_labels, test_data, nWords, false), ...
    @(training_data, training_labels, test_data, nWords) ...
        run_knn(training_data, training_labels, test_data, nWords, KNN_K) ...
    @(training_data, training_labels, test_data, nWords) ...
        run_svm(training_data, training_labels, test_data, nWords, 'linear'), ...
};
nrAlgorithms = size(algorithms,2);

%
% In-domain

kValues = {'MatData/data_100.mat','MatData/data_250.mat','MatData/data_500.mat','MatData/data_750.mat', ...
            'MatData/data_1000.mat','MatData/data_1250.mat', 'MatData/data_1500.mat', ...
            'MatData/data_1750.mat' , 'MatData/data_2000.mat', 'MatData/data_2250.mat','MatData/data_2500.mat'};

error_per_feature_matrix = zeros(length(kValues), nrAlgorithms);
timing_per_feature_matrix = zeros(length(kValues), nrAlgorithms);
stddev_per_feature_matrix = zeros(length(kValues), nrAlgorithms);

nLabels = 6;
tic;
for dataId = 1:length(kValues)
    
    disp(sprintf('Testing k = %d features', dataId));
    clear wordcount
    clear labels_sentiment
    clear labels_classes
        
    load(kValues{dataId});
    nWords = max(cellfun(@(x) max(x.id), wordcount));
    
    [errors, stddevs, timings] = indomain_task(algorithms, wordcount, labels_sentiment, labels_classes, nWords);
    
    error_per_feature_matrix(dataId, :) = errors;
    stddev_per_feature_matrix(dataId, :) = stddevs;
    timing_per_feature_matrix(dataId, :) = timings;
    
    disp(sprintf('Making backup of k = %d', dataId));
    save('backup_feature.mat', 'error_per_feature_matrix','stddev_per_feature_matrix','timing_per_feature_matrix');
    
end
t = toc;

%% Plotting (can be run independently of above code if one sets nrAlgorithms).

load('../Plottar/backup_feature_bigram.mat');
if ~exist('nrAlgorithms')
    nrAlgorithms = 6;
end

xvals = repmat([100, 250, 500, 750, 1000, 1250 ,1500, 1750, 2000, 2250, 2500]', [1, nrAlgorithms]);
errorbar(xvals, error_per_feature_matrix, 0.5*stddev_per_feature_matrix)
legend('Perceptron', 'Averaged Perceptron', 'Naive Bayes TFIDF', 'Naive Bayes Bin', 'KNN', 'SVM');
xlabel('Feature-size (# words)')
ylabel('Misclassification')
title('Algorithm classification comparison for different feature set sizes')
axis([0 2500 0.35 0.56])
%% Plotting of timing

clear
load('../Plottar/backup_feature_bigram.mat');
if ~exist('nrAlgorithms')
    nrAlgorithms = 6;
end

xvals = repmat([100, 250, 500, 750, 1000, 1250 ,1500, 1750, 2000, 2250, 2500]', [1, nrAlgorithms]);
plot(xvals, timing_per_feature_matrix)
legend('Perceptron', 'Averaged Perceptron', 'Naive Bayes TFIDF', 'Naive Bayes Bin', 'KNN', 'SVM');
xlabel('Feature-size (# words)')
ylabel('Timing (s)')
title('Algorithm timing comparison for different feature set sizes')
axis([0 2500 0 20])