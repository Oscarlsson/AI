clear;
load ('../data.mat');

nWords = max(cellfun(@(x) max(x.id), wordcount));

labels = labels_classes; % Change to labels_sentiment for sentiment classification

classifications = ...
    run_naivebayes(wordcount, labels, wordcount, nWords);

disp(sprintf('Classified correctly: %1.2f percent.', ...
    sum(classifications == labels) / length(labels)));
