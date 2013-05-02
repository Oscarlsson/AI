clear
% Init data and paths.
addpath('Percetron')
addpath('Naive Bayes')
addpath('K Nearest')

N_PERCEPTRON_ITERATIONS = 25;
N_AVG_PERCEPTRON_ITERATIONS = 25;
KNN_K = 20;

% No SVM, and No binary NB, no perceptron (only average)
algorithms = { ...
    @(training_data, training_labels, test_data, nWords) ...
        run_average_perceptron(training_data, training_labels, test_data, N_AVG_PERCEPTRON_ITERATIONS, nWords), ...
    @(training_data, training_labels, test_data, nWords) ...
        run_naivebayes(training_data, training_labels, test_data, nWords, true) ...
};
nrAlgorithms = size(algorithms,2);

%
% In-domain
nLabels = 6;

classification_matrix = zeros(nLabels, nLabels, nrAlgorithms);
tic;
for algoId = 1:nrAlgorithms
    
    if algoId == 3
        disp('Running KNN with 100');
        clear wordcount
        clear labels_sentiment
        clear labels_classes
        load MatData/data_100.mat
        nWords = 100;
    else
        disp('Running any other alg with 2000');
        clear wordcount
        clear labels_sentiment
        clear labels_classes
        load MatData/data_2000.mat
        nWords = 2000;
    end
        
   category_data = wordcount;
   category_labels = labels_classes;
   % DEBUG: Use subset of data...
%    subsetIndices = rand(1,length(wordcount)) < 0.5;
%    category_data = category_data(subsetIndices);
%    category_labels = category_labels(subsetIndices);
   [~, ~, ~, classifications] = ...
       test_algorithm( algorithms{algoId}, category_data, ...
       category_labels, category_data, category_labels, 1, nWords);
   for i = 1:length(classifications)
       realClass = category_labels(i);
       guessedClass = classifications(i);
       classification_matrix(realClass, guessedClass, algoId) = ...
           classification_matrix(realClass, guessedClass, algoId) + 1;
   end
   
   disp(sprintf('Making backup of algo id = %d', algoId));
   save('backup_textclassification.mat', 'classification_matrix');
end
t = toc;

%% Plotting
clear;
load('../Plottar/backup_textclassification.mat')
nrAlgorithms = 2;
nLabels = 6;
figure
classification_matrix_sum = zeros(6,6);
for algoId = 1:nrAlgorithms
    classification_matrix_sum = ...
        classification_matrix_sum + 1/nrAlgorithms * classification_matrix(:,:,algoId);
end
% Normalize
classification_matrix_sum = classification_matrix_sum ./ repmat(sum(classification_matrix_sum), nLabels, 1)
% Plot
bar(classification_matrix_sum,'DisplayName','classification_matrix_sum')
%legend('Camera', 'Books', 'DVD', 'Health', 'Music', 'Software');
h = legend('Camera', 'Books', 'DVD', 'Health', 'Music', 'Software', 'location', [0.122 0.78 0.1 0.1]);
v = get(h,'title');
set(v,'string','Classified as');

axis([0 7 0 0.1])

set(gca,'XTickLabel',{'Camera', 'Books', 'DVD', 'Health', 'Music', 'Software'})
ylabel('Classifications (%)')
xlabel('Test document of class')
title('Text classification results')