clear;
load ('../data.mat');

classifications = ...
    run_naivebayes(wordcount, labels_sentiment, wordcount, 2000);

realClasses = labels_sentiment;

disp(sprintf('Classified correctly: %1.2f percent.', ...
    sum(classifications == realClasses) / length(realClasses)));
