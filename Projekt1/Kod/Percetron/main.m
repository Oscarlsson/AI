clear;
load('../data.mat');

jmax = 100;
error_rate_sent = zeros(jmax,1);
error_rate_sent_aver = zeros(jmax,1);
error_rate_class = zeros(jmax,1);
error_rate_class_aver = zeros(jmax,1);
CVP = cvpartition(size(wordcount,2), 'k', 10);

%plot error rate for different j (number of iterations)
for j = 1:jmax

    for i = 1:CVP.NumTestSets

        %classification sentiment
        training_data = wordcount(CVP.training(i));
        training_targets = labels_sentiment(CVP.training(i));
        test_data = wordcount(CVP.test(i));
        test_targets = labels_sentiment(CVP.test(i));
        
        classifications = run_average_perceptron(training_data, training_targets, test_data, true, j);
        correctFraction_sent_aver = sum(classifications == test_targets) / length(test_targets);
        disp(sprintf('(AP) Classified correctly: %1.2f.', correctFraction));
        
        classifications = run_perceptron(training_data, training_targets, test_data, true, j);
        correctFraction_sent = sum(classifications == test_targets) / length(test_targets);
        disp(sprintf('(P) Classified correctly: %1.2f.', correctFraction)); 
        
        %classification over classes 
        training_targets = labels_classes(CVP.training(i));
        test_targets = labels_classes(CVP.test(i));
        
        classifications = run_average_perceptron(training_data, training_targets, test_data, false, j);
        correctFraction_class_aver = sum(classifications == test_targets) / length(test_targets);
        disp(sprintf('(AP) Classified correctly: %1.2f.', correctFraction));
        
        classifications = run_perceptron(training_data, training_targets, test_data, false, j);
        correctFraction_class = sum(classifications == test_targets) / length(test_targets);
        disp(sprintf('(P) Classified correctly: %1.2f.', correctFraction)); 
        
        error_rate_sent(j) = error_rate_sent(j) + correctFraction_sent;
        error_rate_sent_aver(j) = error_rate_sent_aver(j) + correctFraction_sent_aver;
        error_rate_class(j) = error_rate_class(j) + correctFraction_class;
        error_rate_class_aver(j) = error_rate_class_aver(j) + correctFraction_class_aver;
        
    end
    error_rate_sent(j) = error_rate_sent(j)/CVP.NumTestSets;
    error_rate_sent_aver(j) = error_rate_sent_aver(j)/CVP.NumTestSets;
    error_rate_class(j) = error_rate_class(j)/CVP.NumTestSets;
    error_rate_class_aver(j) = error_rate_class_aver(j)/CVP.NumTestSets;

end

x = 1:jmax;
plot(x, error_rate_sent)
hold on;
plot(x, error_rate_sent_aver)
hold on;
plot(x, error_rate_class)
hold on;
plot(x, error_rate_class_aver)



