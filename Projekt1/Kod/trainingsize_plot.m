%% This file runs the different algos on some datasets...
clear
% Init data and paths.
load MatData/data_2000.mat
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

tss_values = linspace(0.1,1,10);
errorAlg = zeros(nrAlgorithms,1);
timingAlg= zeros(nrAlgorithms,1);

outputArrayAlg = zeros(length(tss_values), nrAlgorithms);
outputArrayTiming = zeros(length(tss_values), nrAlgorithms);

nWords = max(cellfun(@(x) max(x.id), wordcount));

for tss_index = 1:length(tss_values)
    tss_value = tss_values(tss_index)
             %save('backup_trainingsize.mat', 'outputArrayAlg', 'tss_index', 'outputArrayTiming');

    for algoIndex = 1:nrAlgorithms
        if algoIndex == 5
            disp('Running KNN with 250');
            clear wordcount
            clear labels_sentiment
            clear labels_classes
            load MatData/data_250.mat
        else
            disp('Running any other alg with 2000');
            clear wordcount
            clear labels_sentiment
            clear labels_classes
            load MatData/data_2000.mat
        end
      % save('backup_trainingsize.mat', 'outputArrayAlg', 'tss_index', 'outputArrayTiming');   
        for i = 1:nLabels
           category_data = wordcount(labels_classes == i);
           category_labels = labels_sentiment(labels_classes == i);
           [error, timing] = test_algorithm( algorithms{algoIndex}, category_data, category_labels, category_data, category_labels, tss_value, nWords);
           timingAlg(i) = timing;
           errorAlg(i) = error;
        end
        
        %Average over all labels in-domain
        outputArrayAlg(tss_index, algoIndex) = mean(errorAlg);
        outputArrayTiming(tss_index, algoIndex) = mean(timingAlg);

        save('backup_trainingsize.mat', 'outputArrayAlg', 'tss_index', 'outputArrayTiming');
    end
end

% Correct? No.
plot(tss_values, outputArrayAlg)
legend('Perceptron', 'Averaged Perceptron', 'Naive Bayes TFIDF', 'Naive Bayes Bin', 'KNN', 'SVM');