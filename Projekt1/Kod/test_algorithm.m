function [ error, stddev, timing, classificationsResult ] = test_algorithm( algorithm_handle, training_dataset, training_dataset_labels, test_dataset, test_dataset_labels, training_dataset_size_fraction, nWords )

errors = zeros(1,10);
timings = zeros(1,10);

classificationsResult = zeros(size(test_dataset_labels));

% 10-fold CV
cvSetSize = min(size(test_dataset,2), size(training_dataset,2));
CVP = cvpartition(cvSetSize, 'k', 10);
for i = 1:CVP.NumTestSets
%for i = 1:1

    % Training data
    training_data = training_dataset(CVP.training(i));
    training_targets = training_dataset_labels(CVP.training(i));
    
    test_data = test_dataset(CVP.test(i));
    test_targets = test_dataset_labels(CVP.test(i));
    
    % DEBUG
    % Only use a subset of training/test data to decrease runtime...
%     training_data = training_data(1:500);
%     training_targets = training_targets(1:500);
%     test_data = test_data(1:50);
%     test_targets = test_targets(1:50);
    
    training_data_size = ceil(training_dataset_size_fraction * size(training_data, 2));
    tic;
    classifications = feval(algorithm_handle, training_data(1:training_data_size), ...
        training_targets(1:training_data_size), test_data, nWords);
        
    classificationsResult(CVP.test(i)) = classifications;
    
    timings(i) = toc;
    errors(i) = sum(classifications ~= test_targets) / length(test_targets);
     
    [timings(i), errors(i)];
end

timing = mean(timings);
error = mean(errors);
stddev = std(errors);

end

