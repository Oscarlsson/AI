function [ class ] = NaiveBayesImpl( document, Pc, Pwc, WORDINDEXCORR )
%NAIVEBAYES This method returns a class label for the input args
%   Detailed explanation goes here

% Select from Pwc = P(w|c) only those words that occur in
% the document (by document.id).
wordProbabilities = Pwc(document.id + WORDINDEXCORR, :);

% Sum logs instead of multiplying probabilities.
% Also, calculate all classes at once.
% Pcd = P(c | document)
Pcd = log(Pc) + sum( log(wordProbabilities) );

[maxPcd, maxPcdIndex] = max(Pcd);

class = maxPcdIndex;

end

