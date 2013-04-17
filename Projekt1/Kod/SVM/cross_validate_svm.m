function [ error ] = cross_validate_svm( data, targets, kernel_function, optimization_method, showplot)
% If the parameter 'showplot' is true, a plot will update with each
% iteration
CVP = cvpartition(size(data,1), 'k', 10);

totalErrors = 0;

for i = 1:CVP.NumTestSets
    % Training data
    training_data = data(CVP.training(i), :);
    training_targets = targets(CVP.training(i));
    
    % Validation data
    test_data = data(CVP.test(i), :);
    test_targets = targets(CVP.test(i), :);
    svm_model = svmtrain(training_data, training_targets, ...
        'method', optimization_method, ...
        'Kernel_Function', kernel_function, 'showplot', showplot);
    if (showplot)
        drawnow expose;
    end
    totalErrors = totalErrors + sum(svmclassify(svm_model, test_data) ~= test_targets);
end

error = totalErrors / size(data, 1);

end