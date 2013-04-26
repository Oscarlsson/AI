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
        run_naivebayes(training_data, training_labels, test_data, nWords, true), ...
    @(training_data, training_labels, test_data) ...
        run_naivebayes(training_data, training_labels, test_data, nWords, false), ...
    @(training_data, training_labels, test_data) ...
        run_average_perceptron(training_data, training_labels, test_data, N_AVG_PERCEPTRON_ITERATIONS, nWords), ...
    @(training_data, training_labels, test_data) ...
        run_knn(training_data, training_labels, test_data, nWords, KNN_K), ...
    @(training_data, training_labels, test_data) ...
        run_svm(training_data, training_labels, test_data, nWords, 'linear') ...
};


%
% In-domain
%
nLabels = 6;

%errorCatSent = zeros(size(algorithms,2),nLabels);
%timeCatSent = zeros(size(algorithms,2),nLabels);

%for i = 1:nLabels
%    for algoIndex = 1:size(algorithms,2)
        
        %Plocka ut kategorin
%        category_data = wordcount(labels_classes == i);
%        category_labels = labels_sentiment(labels_classes == i);
        
%        [error, timing] = test_algorithm( algorithms{algoIndex}, category_data, category_labels, category_data, category_labels, 1);
%        errorCatSent(algoIndex, i) = error;
%        timeCatSent(algoIndex, i)  = timing;

%    end
%end
%meanError = zeros(nLabels,1);
%for i = 1:nLabels
%    meanError(i) = mean(errorCatSent(:,i))
%end
%[valuemin, indexmin] = min(meanError);
%[valuemax, indexmax] = max(meanError);

%disp(sprintf('Best category is %i with %1.2f', indexmin, valuemin));
%disp(sprintf('Worst category is %i with %1.2f', indexmax, valuemax));


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
            [error, timing] = test_algorithm( algorithms{algoIndex}, training_data, training_labels, test_data, test_labels, 1);
            errorCatSent(algoIndex, i,j) = error;
            timeCatSent(algoIndex, i,j)  = timing;

        end
    end
end

% Text Categorization
%
 for algoIndex = 1:size(algorithms,2)
          
        [error, timing] = test_algorithm( algorithms{algoIndex}, wordcount, labels_classes, wordcount, labels_classes, 1);
        errorCatSent(algoIndex, i) = error;
        timeCatSent(algoIndex, i)  = timing;

end

%Text categorization - categorizera mina dokument i alla kategorier och
%välj den som är mest lämplig?
