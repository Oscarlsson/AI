function [w] = perceptron(data,Y)  

[n, d] = size(data);

%Create the randomized w.
w = zeros(d,1);

%
%PERCEPTRON ALGORITHM
%
for j = 1:50 %number of iterations
      for i = 1:n
          if sign((data(i,:)*w)') ~= Y(i) %makes mistake?
              w = w + (Y(i) * data(i,:))'; %update w
          end
      end
end

w = w/norm(w);

end
