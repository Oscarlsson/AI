%average perceptron
function [wa] = averaged_perceptron(data, labels, N)

[n, d] = size(data);
%N = 50;
count = 0;

%Initialize w and wa.
w = zeros(d,1);
wa = zeros(d,1);

%
%AVERAGED PERCEPTRON ALGORITHM
%
validationErrors = [];
trainingErrors = [];
for j = 1:N %number of iterations
%    validationErrors = [validationErrors, ...
%        classification_error(w, validationData, validationLabels)];
%    trainingErrors = [trainingErrors, ...
%        classification_error(w, trainingData, trainingLabels)];
      for i = 1:n
          if sign((data(i,:)*w)') ~= labels(i) %makes mistake?
              w = w + (labels(i) * data(i,:))'; %update w
              average_weight = (N*n - count) / (N*n);
              wa = wa + average_weight * (labels(i) * data(i,:))';
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
