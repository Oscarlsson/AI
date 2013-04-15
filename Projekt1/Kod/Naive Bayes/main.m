%% Setup, data.

clear;
wordcount = fmatrix('../Datamanipulation/output_final.txt', 0);
filenames = importdata('../Datamanipulation/filenames_final.txt');
 
% Delete unused documents. Does not have any other implications.
emptyDocuments = cellfun(@(x) length(x.id) == 0, wordcount);
wordcount(emptyDocuments) = [];
filenames(emptyDocuments) = [];

nDocuments = length(filenames);
%%
% TEMP: Change word ids
for i = 1:nDocuments
    wordcount{i}.id = wordcount{i}.id + 1;
end
%%
nWords = max(cellfun(@(x) max(x.id), wordcount));

%% Classes initialization
nClasses = 2;

classes = zeros(1, length(filenames));
% Read filenames, set labels 1 = "neg", 2 = "pos"
for i = 1:length(filenames)
    classes(i) = isempty(strfind(filenames{i}, 'neg')) + 1;
end

%% NB start

% Setup Pc(c) =
%   = P(c) =
%   = (#docs with class c) / (#docs)
Pc = zeros(1,2);
for i = 1:nClasses
    Pc(i) = sum(classes == i) / length(classes);
end

%
laplace = 0.1;
% Setup Pwc(w, c) = P(w | c) = (#words=w in class c) / (#words in class c)
Pwc = laplace*ones(nWords, nClasses); % Note: ones. Not zeroes. Laplace smoothing.
for d = 1:nDocuments
    class = classes(d);
    for i = 1:length(wordcount{d}.id)
        docWordId = wordcount{d}.id(i);
        docWordCount = wordcount{d}.cnt(i);
        Pwc(docWordId, class) = ...
            Pwc(docWordId, class) + docWordCount;
    end
end
% Normalize Pwc: Divide each element by the column sum.
% Due to Laplace smoothing, add +nWords to denominator.
% See http://nlp.stanford.edu/IR-book/html/htmledition/naive-bayes-text-classification-1.html
Pwc = Pwc ./ (ones(nWords, 1) * sum(Pwc) + nWords*laplace);

% DEBUG, not pretty.
test;
disp(sprintf('Classified correctly: %1.2f percent.', (nCorrect / (nWrong + nCorrect)) * 100));
