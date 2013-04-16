function [w, iter_count] = perceptron(data,Y)  


[n, d] = size(data);

%Create the randomized w.
w = randn(d,1);

%
%PERCEPTRON ALGORITHM
%

iter_count = 0;
not_converged = true;

while not_converged
      not_converged = false;
      for i = 1:n
          if sign(w'*data(i,:)') ~= Y(i,:)
              %added transpose to get correct dim. 
              %Change depending on data.
              
              w = w + (Y(i) * data(i,:))';
              not_converged = true;
          end
      end
      iter_count = iter_count +1;
end

w = w/norm(w);
