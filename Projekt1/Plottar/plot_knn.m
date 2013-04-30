load knn_2000words_testdata100unigram.mat

hold on;
plot(x, 1-classes_error_rate, 'b')
plot(x, 1-sentimental_error_rate, 'r')
legend('Classes', 'Sentimental')
xlabel('# Neighbours')
ylabel('Misclassification (%)')