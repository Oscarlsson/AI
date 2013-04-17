%TODO: fixa plot för att visa hur många iterationer som behövs innan den konvergerar. 
% träna på allt och testa på allt ger runt 74%

function [ classifications ] = run_perceptron(training_data, training_labels, test_data) 

nTrainingDocuments = length(training_data);
nTestDocuments = length(test_data);

max_id = 0; 

%find highest word id
for i = 1 : nTrainingDocuments
   idarray = training_data{i}.id;
   max_elem = max(idarray);
   if (max_elem > max_id)
      max_id = max_elem ; 
   end 
end
max_id;

%initalize TRAINING data matrix for perceptron
%update TRAINING data matrix with cnt, for each document
trainingData = zeros(nTrainingDocuments,max_id); 
for i = 1 : nTrainingDocuments
    id = training_data{i}.id; 
    cnt = training_data{i}.cnt;
    for j = 1 : length(id)
        trainingData(i,id(j)) = cnt(j);
    end   
end

%initalize TEST data matrix for perceptron
%update TEST data matrix with cnt, for each document
testData = zeros(nTestDocuments,max_id); 
for i = 1 : nTestDocuments
    id = test_data{i}.id; 
    cnt = test_data{i}.cnt;
    for j = 1 : length(id)
        testData(i,id(j)) = cnt(j);
    end   
end

% (-1) <- (2)
% ( 1) <- (1)
classes = 1*(training_labels == 1) + (-1)*(training_labels == 2);
w = perceptron(trainingData, classes);

classSigns = sign((testData*w)');

% (-1) -> (2)
% ( 1) -> (1)
classifications = 1*(classSigns == 1) + 2*(classSigns == -1);

end 
