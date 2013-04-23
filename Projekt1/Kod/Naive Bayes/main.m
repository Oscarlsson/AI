clear;
load ('../data.mat');

nWords = max(cellfun(@(x) max(x.id), wordcount));

classifications = ...
    run_naivebayes(wordcount, labels_sentiment, wordcount, nWords);

realClasses = labels_sentiment;

disp(sprintf('Classified correctly: %1.2f percent.', ...
    sum(classifications == realClasses) / length(realClasses)));
