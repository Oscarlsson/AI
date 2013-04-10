clear;
wordcount = fmatrix('../output_final.txt');
filenames = importdata('../filenames_final.txt');
 
emptyDocuments = cellfun(@(x) length(x.id) == 0, wordcount);
wordcount(emptyDocuments) = [];
filenames(emptyDocuments) = [];

% TODO: Change the preprocessing Python script to 
% output word indices from 1:N instead of 0:N-1 .
MINWORDINDEX = 0;
if MINWORDINDEX == 0
    WORDINDEXCORR = 1;
else
    WORDINDEXCORR = 0;
end

nClasses = 2;
nWords = max(cellfun(@(x) max(x.id), wordcount)) + WORDINDEXCORR;
nDocuments = length(filenames);

classes = zeros(1, length(filenames));
% Read filenames, set labels 1 = "neg", 2 = "pos"
for i = 1:length(filenames)
    classes(i) = isempty(strfind(filenames{i}, 'neg')) + 1;
end

% Setup Pc(c) =
%   = P(c) =
%   = (#docs with class c) / (#docs)
Pc = zeros(1,2);
for i = 1:nClasses
    Pc(i) = sum(classes == i) / length(classes);
end

% Setup Pwc(w, c) = P(w | c) = (#words=w in class c) / (#words in class c)
Pwc = ones(nWords, nClasses); % Note: ones. Not zeroes. Laplace smoothing.
for d = 1:nDocuments
    class = classes(d);
    for w = 1:length(wordcount{d}.id)
        wordId = wordcount{d}.id(w) + WORDINDEXCORR;
        wordCount = wordcount{d}.cnt(w);
        Pwc(wordId, class) = ...
            Pwc(wordId, class) + wordCount;
    end
end
% Normalize Pwc: Divide each element by the column sum.
% Due to Laplace smoothing, add +nWords to denominator.
% See http://nlp.stanford.edu/IR-book/html/htmledition/naive-bayes-text-classification-1.html
Pwc = Pwc ./ (ones(nWords, 1) * sum(Pwc) + nWords);

% DEBUG, not pretty.
test;
disp(sprintf('Classified correctly: %1.2f percent.', (nCorrect / (nWrong + nCorrect)) * 100));
