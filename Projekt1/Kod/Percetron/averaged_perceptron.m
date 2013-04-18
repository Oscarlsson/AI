%average perceptron
function [wa] = averaged_perceptron(data,Y)  

[n, d] = size(data);
N = 50;
count = 0;

%Create the randomized w.
w = zeros(d,1);
wa = zeros(d,1);

%
%PERCEPTRON ALGORITHM
%
for j = 1:N %number of iterations
      for i = 1:n
          if sign((data(i,:)*w)') ~= Y(i) %makes mistake?
              w = w + (Y(i) * data(i,:))'; %update w
              average_weight = (N*n - count) / (N*n);
              wa = wa + average_weight * (Y(i) * data(i,:))';
          end
          count = count + 1;
      end
end
wa = wa/norm(wa);
end

