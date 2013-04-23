function [ error ] = classification_error( w, data, labels )
%CLASSIFICATION_ERROR Summary of this function goes here
%   Detailed explanation goes here
nError = 0;
for i = 1:size(data,1)
    if sign((data(i,:)*w)') ~= labels(i)
        nError = nError + 1;
    end
end

error = nError / size(data, 1);

end