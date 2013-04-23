function [ classifications ] = run_perceptron(training_data, training_labels, test_data, sentiment, N) 

[trainingData, testData] = init_trainingdata_and_testdata(training_data, test_data);

if sentiment
    % (-1) <- (2)
    % ( 1) <- (1)
    classes = 1*(training_labels == 1) + (-1)*(training_labels == 2);

    w = perceptron(trainingData, classes, N);
    classSigns = sign((testData*w)');
    % (-1) -> (2)
    % ( 1) -> (1)
    classifications = 1*(classSigns == 1) + 2*(classSigns == -1);
    
else
    classes = zeros(6,length(training_labels));
    w = zeros(size(trainingData,2),6);
    
    for i = 1:6
        classes(i,:) = 1/i*(training_labels == i) + (-1/i)*(training_labels ~= i);
        w(:,i) = perceptron(trainingData, classes(i,:), N);
        classSigns(i,:) = (testData*w(:,i))';
    end
    
    classifications = zeros(1,size(classSigns,2));
    for j = 1:size(classSigns,2)
        max_value = max(classSigns, [], 1);
        classifications(1,j) = find(classSigns(:,j) == max_value(j));
    end
end

end
