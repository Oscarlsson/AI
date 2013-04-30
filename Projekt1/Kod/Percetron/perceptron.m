function [w] = perceptron(data, Y, N)  

[n, d] = size(data);
%N = 50;

%Initialize w
w = zeros(d,1);

%
%PERCEPTRON ALGORITHM
%
for j = 1:N %number of iterations
    %classificationerror = classification_error(w, data, Y);
      for i = 1:n
          if sign((data(i,:)*w)') ~= Y(i) %makes mistake?
              w = w + (Y(i) * data(i,:))'; %update w
          end
      end
end

w = w/norm(w);

end
