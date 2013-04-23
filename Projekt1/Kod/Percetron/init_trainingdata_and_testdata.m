function [trainingData, testData] = init_trainingdata_and_testdata(training_data, test_data)

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

end
