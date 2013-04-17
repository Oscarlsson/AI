%% Setup, data.

clear;
wordcount = fmatrix('output_final.txt', 0);
filenames = importdata('filenames_final.txt');

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

X = zeros(nDocuments, nWords);
for d = 1:nDocuments
    for i = 1:size(wordcount{i}.id)
        w = wordcount{d}.id(i);
        tfidf = wordcount{d}.cnt(i);
        X(d,w) = tfidf;
    end
end
Y = classes';

