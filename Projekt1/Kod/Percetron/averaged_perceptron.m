%average perceptron
function [wa] = averaged_perceptron(data, labels)

[trainingData, trainingLabels, validationData, validationLabels] = ...
    training_validation_partition(data, labels, 10);

[n, d] = size(trainingData);

N = 20;
count = 0;

%Create the randomized w.
w = zeros(d,1);
wa = zeros(d,1);

%
%PERCEPTRON ALGORITHM
%
validationErrors = [];
trainingErrors = [];
for j = 1:N %number of iterations
%    validationErrors = [validationErrors, ...
%        classification_error(w, validationData, validationLabels)];
%    trainingErrors = [trainingErrors, ...
%        classification_error(w, trainingData, trainingLabels)];
      for i = 1:n
          if sign((trainingData(i,:)*w)') ~= trainingLabels(i) %makes mistake?
              w = w + (trainingLabels(i) * trainingData(i,:))'; %update w
              average_weight = (N*n - count) / (N*n);
              wa = wa + average_weight * (trainingLabels(i) * trainingData(i,:))';
          end
          count = count + 1;
      end
end

%hold on
%    plot(validationErrors, 'r')
%    plot(trainingErrors, 'b')
%hold off

wa = wa/norm(wa);
end

