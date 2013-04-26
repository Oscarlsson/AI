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
%    @(training_data, training_labels, test_data, nWords) ...
%        run_svm(training_data, training_labels, test_data, nWords, 'linear'), ...
    @(training_data, training_labels, test_data, nWords) ...
        run_perceptron(training_data, training_labels, test_data, N_PERCEPTRON_ITERATIONS, nWords), ...
    @(training_data, training_labels, test_data, nWords) ...
        run_naivebayes(training_data, training_labels, test_data, nWords, true), ...
    @(training_data, training_labels, test_data, nWords) ...
        run_naivebayes(training_data, training_labels, test_data, nWords, false), ...
    @(training_data, training_labels, test_data, nWords) ...
        run_average_perceptron(training_data, training_labels, test_data, N_AVG_PERCEPTRON_ITERATIONS, nWords), ...
    @(training_data, training_labels, test_data, nWords) ...
        run_knn(training_data, training_labels, test_data, nWords, KNN_K) ...
};
nAlgorithms = size(algorithms,2);

%
% In-domain
%
kValues = {'MatData/data_100.mat', 'MatData/data_500.mat', 'MatData/data_1000.mat', 'MatData/data_1500.mat', 'MatData/data_2000.mat'};
error_per_feature_matrix = zeros(length(kValues), nAlgorithms);
timing_per_feature_matrix = zeros(length(kValues), nAlgorithms);
stddev_per_feature_matrix = zeros(length(kValues), nAlgorithms);

nLabels = 6;

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
end

%% Plotting
% TODO: 0.01*stddev is only temporary since we only run CV once
xvals = repmat([100, 500, 1000, 1500, 2000]', [1, nAlgorithms]);
errorbar(xvals, error_per_feature_matrix, 0.01*stddev_per_feature_matrix)
legend('Perceptron', 'Naive Bayes TFIDF', 'Naive Bayes Bin', 'Averaged Perceptron', 'KNN'); % SVM