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


%
% In-domain
%
nLabels = 6;
nrAlgorithms = size(algorithms,2);

tss_values = linspace(0.2,1,5);
errorAlg = zeros(nrAlgorithms,1);
outputArray = zeros(length(tss_values), nrAlgorithms);

nWords = max(cellfun(@(x) max(x.id), wordcount));

for tss_index = 1:length(tss_values)
    tss_value = tss_values(tss_index)
    
    for algoIndex = 1:nrAlgorithms
        
        for i = 1:nLabels
           category_data = wordcount(labels_classes == i);
           category_labels = labels_sentiment(labels_classes == i);
           [error, timing] = test_algorithm( algorithms{algoIndex}, category_data, category_labels, category_data, category_labels, tss_value, nWords);
        
           errorAlg(i) = error;
        end
        
        %Average over all labels in-domain
        outputArray(tss_index, algoIndex) = mean(errorAlg);
        
        save('backup_trainingsize.mat', 'outputArray', 'tss_index');
    end
end

% Correct? No.
plot(tss_values, outputArray)