load('../data.mat');

classifications = run_perceptron(wordcount, labels_sentiment, wordcount);

disp(sprintf('Classified correctly: %1.2f percent.', ...
    sum(labels_sentiment == classifications) / length(labels_sentiment)))
    
classifications = run_average_perceptron(wordcount, labels_sentiment, wordcount);

disp(sprintf('Classified correctly: %1.2f percent.', ...
    sum(labels_sentiment == classifications) / length(labels_sentiment)))

