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

nLabels = 6;

%
% Out-of-domain
% Gör inte detta in-domain också????
%
disp('starting out of domain..')
errorCatSent = zeros(size(algorithms,2),nLabels, nLabels);
timeCatSent = zeros(size(algorithms,2),nLabels, nLabels);


for i = 1:nLabels % TRAINING DATA
    disp(sprintf('Training category %i', i));
    training_data   = wordcount(labels_classes == i);
    training_labels = labels_sentiment(labels_classes == i);

    for j = 1:nLabels %TEST DATA
        
        disp(sprintf('Testing on category %i', j));
        test_data   = wordcount(labels_classes == j);
        test_labels = labels_sentiment(labels_classes == j);
   
        for algoIndex = 1:size(algorithms,2)
            [error, timing] = test_algorithm( algorithms{algoIndex}, training_data, training_labels, test_data, test_labels, 1,nWords);
            errorCatSent(algoIndex, i,j) = error;
            timeCatSent(algoIndex, i,j)  = timing;

        end
    end
    save('backup_outofdomain.mat', 'errorCatSent','timeCatSent');
end

%% Plot Skriv kod för plot här.
load('../Plottar/backup_outofdomain.mat')
figure
errorCatSent_sum = zeros(6,6);
for algoId = [2,3]
    errorCatSent_sum = ...
        errorCatSent_sum + 1/2 * reshape(errorCatSent(algoId,:,:), 6, 6);
end
bar(errorCatSent_sum ,'DisplayName','classification_matrix_sum')
h = legend('Camera', 'Books', 'DVD', 'Health', 'Music', 'Software', 'location', [0.125 0.78 0.1 0.1]);
v = get(h,'title');
set(v,'string','Testing on category');


set(gca,'XTickLabel',{'Camera', 'Books', 'DVD', 'Health', 'Music', 'Software'})
ylabel('Classified correctly (%)')
xlabel('Training on category')
title('Out-of-domain sentiment analysis')