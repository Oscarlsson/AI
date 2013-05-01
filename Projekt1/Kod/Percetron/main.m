% main.m consists of test code and is not necessary for any project results
clear;
load('../data.mat');

kmax = 100;

error_rate_sent = zeros(kmax,1);
error_rate_sent_aver = zeros(kmax,1);
error_rate_class = zeros(kmax,1);
error_rate_class_aver = zeros(kmax,1);

CVP = cvpartition(size(wordcount,2), 'k', 10);

%plot error rate for different k (number of iterations)
for k = 1:kmax
    k

    for i = 1:CVP.NumTestSets

        %classification sentiment
        training_data = wordcount(CVP.training(i));
        training_targets = labels_sentiment(CVP.training(i));
        test_data = wordcount(CVP.test(i));
        test_targets = labels_sentiment(CVP.test(i));
        
        %average perceptron
        classifications = run_average_perceptron(training_data, training_targets, test_data, k);
        correctFraction_sent_aver = sum(classifications == test_targets) / length(test_targets);
        disp(sprintf('(AP) Classified correctly: %1.2f.', correctFraction_sent_aver));
        
        %perceptron
        classifications = run_perceptron(training_data, training_targets, test_data, k);
        correctFraction_sent = sum(classifications == test_targets) / length(test_targets);
        disp(sprintf('(P) Classified correctly: %1.2f.', correctFraction_sent)); 
        
        %classification over classes 
        training_targets = labels_classes(CVP.training(i));
        test_targets = labels_classes(CVP.test(i));
        
        %average perceptron
        classifications = run_average_perceptron(training_data, training_targets, test_data, k);
        correctFraction_class_aver = sum(classifications == test_targets) / length(test_targets);
        disp(sprintf('(AP) Classified correctly: %1.2f.', correctFraction_class_aver));
        
        %perceptron
        classifications = run_perceptron(training_data, training_targets, test_data, k);
        correctFraction_class = sum(classifications == test_targets) / length(test_targets);
        disp(sprintf('(P) Classified correctly: %1.2f.', correctFraction_class)); 
        
        %save cv-error
        error_rate_sent(k) = error_rate_sent(k) + correctFraction_sent;
        error_rate_sent_aver(k) = error_rate_sent_aver(k) + correctFraction_sent_aver;
        error_rate_class(k) = error_rate_class(k) + correctFraction_class;
        error_rate_class_aver(k) = error_rate_class_aver(k) + correctFraction_class_aver;
        
    end
    %scale cv-error
    error_rate_sent(k) = error_rate_sent(k)/CVP.NumTestSets;
    error_rate_sent_aver(k) = error_rate_sent_aver(k)/CVP.NumTestSets;
    error_rate_class(k) = error_rate_class(k)/CVP.NumTestSets;
    error_rate_class_aver(k) = error_rate_class_aver(k)/CVP.NumTestSets;

end

x = 1:kmax;
plot(x, error_rate_sent)
hold on;
plot(x, error_rate_sent_aver)
hold on;
plot(x, error_rate_class)
hold on;
plot(x, error_rate_class_aver)



