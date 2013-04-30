load perceptron_2000words_unigram_10foldcv_classes-high_sentimental-low.mat

hold on
plot(x, 1-error_rate_class , 'b')
plot(x, 1-error_rate_sent , 'c')
plot(x, 1-error_rate_class_aver, 'r')
plot(x, 1-error_rate_sent_aver, 'm')

xlabel('#Iterations')
ylabel('Misclassification (%)')
legend('Perceptron classes', 'Perceptron sentimental', 'Average Perceptron classes', 'Average Perceptron sentimental')