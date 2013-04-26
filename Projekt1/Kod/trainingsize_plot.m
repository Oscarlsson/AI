%% This file runs the different algos on some datasets...
clear
% Init data and paths.
load data.mat
addpath('Percetron')
addpath('Naive Bayes')
addpath('SVM')
addpath('K Nearest')

N_PERCEPTRON_ITERATIONS = 25;
N_AVG_PERCEPTRON_ITERATIONS = 25;
KNN_K = 20;

nWords = max(cellfun(@(x) max(x.id), wordcount));

algorithms = { ...
    @(training_data, training_labels, test_data) ...
        run_perceptron(training_data, training_labels, test_data, N_PERCEPTRON_ITERATIONS, nWords), ...
    @(training_data, training_labels, test_data) ...
        run_naivebayes(training_data, training_labels, test_data, nWords), ...
    @(training_data, training_labels, test_data) ...
        run_average_perceptron(training_data, training_labels, test_data, N_AVG_PERCEPTRON_ITERATIONS, nWords), ...
    @(training_data, training_labels, test_data) ...
        run_knn(training_data, training_labels, test_data, nWords, KNN_K) ...
};

%
% In-domain
%
nLabels = 6;
nrAlgorithms = size(algorithms,2);

tss_values = linspace(0.2,1,5);
errorAlg = zeros(nrAlgorithms,1);
outputArray = zeros(length(tss_values), nrAlgorithms);

for tss_index = 1:length(tss_values)
    tss_value = tss_values(tss_index)
    
    for algoIndex = 1:nrAlgorithms
        
        for i = 1:nLabels
           category_data = wordcount(labels_classes == i);
           category_labels = labels_sentiment(labels_classes == i);
           [error, timing] = test_algorithm( algorithms{algoIndex}, category_data, category_labels, category_data, category_labels, tss_value);
        
           errorAlg(i) = error;
        end
        
        %Average over all labels in-domain
        outputArray(tss_index, algoIndex) = mean(errorAlg)
    end
end

% Correct? No.
plot(tss_values, outputArray)



