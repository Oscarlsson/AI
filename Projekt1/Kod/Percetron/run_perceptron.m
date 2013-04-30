function [ classifications ] = run_perceptron(training_data, training_labels, test_data, N, nWords) 

[trainingData, testData] = init_trainingdata_and_testdata(training_data, test_data, nWords);
numberOfClasses = length(unique(training_labels));

%sentimental classification or class classification
if numberOfClasses == 2
    % (-1) <- (2)
    % ( 1) <- (1)
    classes = 1*(training_labels == 1) + (-1)*(training_labels == 2);

    w = perceptron(trainingData, classes, N);
    classSigns = sign((testData*w)');
    % (-1) -> (2)
    % ( 1) -> (1)
    classifications = 1*(classSigns == 1) + 2*(classSigns == -1);
    
else

    %initialize classes, w and classSigns (for speed)
    classes = zeros(numberOfClasses,length(training_labels));
    w = zeros(size(trainingData,2),numberOfClasses);
    classSigns = zeros(numberOfClasses,size(testData,1));
    
    %for each class i, train the perceptron and classify the data as i or not i
    for i = 1:numberOfClasses
        classes(i,:) = 1*(training_labels == i) + (-1)*(training_labels ~= i); %divide the data in two classes
        w(:,i) = perceptron(trainingData, classes(i,:), N); %run perceptron
        classSigns(i,:) = (testData*w(:,i))'; %no sign since we should compare the classes
    end
    
    %initialize classifications (for speed)
    classifications = zeros(1,size(classSigns,2));
    
    %get maximum value of each document, i.e. which class it belongs to
    max_value = max(classSigns, [], 1);
    
    %for each doucument, find the class the document belongs to
    for j = 1:size(classifications,2)
        class = find(classSigns(:,j) == max_value(1,j));
        
        %if two classes have the same probability, just classify the document as the first one
        if length(class) > 1
            class = randsample(class,1);
            %class = class(1);
        end
        classifications(1,j) = class;
    end
end

end
