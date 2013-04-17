clear;
wordcount = fmatrix('Datamanipulation/output_final.txt', 0);
filenames = importdata('Datamanipulation/filenames_final.txt');
 
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

% Kör Y.m som är genererad av Melker vid perceptron-grejer.
Y ; 
categories = struct('pos',pos,'camera',camera,'book',book,'dvd',dvd,'health'
                    ,health,'music',music,'software',software) ;  
%
% Sparar wordcount variabeln i en fil som hetet data.mat
%
save("data.mat")
