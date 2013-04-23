%average perceptron
function [wa] = averaged_perceptron(data, labels, N)

[n, d] = size(trainingData);
%N = 50;
count = 0;

%Initialize w and wa.
w = zeros(d,1);
wa = zeros(d,1);

%
%PERCEPTRON ALGORITHM
%
for j = 1:N %number of iterations
    %classification_error(w, validationData, validationLabels);
      for i = 1:n
          if sign((data(i,:)*w)') ~= labels(i) %makes mistake?
              w = w + (labels(i) * data(i,:))'; %update w
              average_weight = (N*n - count) / (N*n);
              wa = wa + average_weight * (labels(i) * data(i,:))';
          end
          count = count + 1;
      end
end
wa = wa/norm(wa);
end
