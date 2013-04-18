%TODO: fixa plot för att visa hur många iterationer som behövs innan den konvergerar. 
% träna på allt och testa på allt ger runt 74%

function [ classifications ] = run_perceptron(training_data, training_labels, test_data) 

[trainingData, testData, classes] = init_trainingdata_and_testdata(training_data, training_labels, test_data);
w = perceptron(trainingData, classes);

disp('size w')
size(w)

classSigns = sign((testData*w)');

% (-1) -> (2)
% ( 1) -> (1)
classifications = 1*(classSigns == 1) + 2*(classSigns == -1);

end 
