disp('Training...');
trainId = rand(nDocuments,1) < 0.2;
nTrains = sum(trainId)
training_data = X(trainId,:);
training_targets = Y(trainId);
svm_model = svmtrain(training_data, training_targets, 'method', 'QP', ...
    'Kernel_Function', 'linear', 'showplot', false);

testId = rand(nDocuments,1) < 0.05;
nTests = sum(testId)
test_data = X(testId,:);
test_targets = Y(testId);
nErrors = sum(svmclassify(svm_model, test_data) ~= test_targets)
nErrors/nTests