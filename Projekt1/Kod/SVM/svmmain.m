%clear
%load d2.mat
%close all

% svmtrain(X, Y, 'method', 'QP', 'Kernel_Function', 'rbf', 'showplot', true);

kernel_methods = {'linear', 'quad', 'rbf'};
optimization_methods = {'QP', 'SMO'};

for km = 1:length(kernel_methods)
    for om = 1:length(optimization_methods)
        tic;
        error = cross_validate_svm(X,Y, ...
            kernel_methods{km}, optimization_methods{om}, false);
        timing_seconds = toc;
        
        disp(sprintf('Kernel: %s \t Optimization: %s \t Error: %.2f \t Timing: %.2f', ...
            kernel_methods{km}, optimization_methods{om}, error, timing_seconds));
    end
end