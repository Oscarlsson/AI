clear;
%wordcount = fmatrix('Datamanipulation/output.txt', 0);
wordcount = fmatrix('output.txt', 0);

% added delimiter to importdata to be compatible with octave
% last cell is crap.
%filenames = importdata('Datamanipulation/filenames.txt',' ');
isOctave = exist('OCTAVE_VERSION') ~= 0;
if (isOctave)
	filenames = importdata_octave('filenames.txt',' ');
else
	filenames = importdata('filenames.txt',' ');
end
 
% Delete unused documents. Does not have any other implications.
emptyDocuments = cellfun(@(x) length(x.id) == 0, wordcount);
wordcount(emptyDocuments) = [];
filenames(emptyDocuments) = [];

nDocuments = length(filenames)-1;

%%
% TEMP: Change word ids
for i = 1:nDocuments
    wordcount{i}.id = wordcount{i}.id + 1;
end
%%
nWords = max(cellfun(@(x) max(x.id), wordcount));

%% Classes initialization

%
% Rewrote gen_Y in this way. 
%
topics = {'pos','camera','book','dvd','health','music','software'};
Ys = zeros(length(topics), nDocuments);
for i = 1:length(topics)
    for j = 1:length(filenames)
        if ~isempty(strfind(filenames{j},topics{i}))
            Ys(i, j) = 1;
        else
            Ys(i,j)  = -1;
        end
    end 
end
                
categories = struct('pos', Ys(1,:),'camera',Ys(2,:),'book',Ys(3,:),'dvd',Ys(4,:),'health'... 
                    ,Ys(5,:),'music',Ys(6,:),'software',Ys(7,:));

labels_sentiment = 1*(categories.pos == 1) + 2*(categories.pos == -1);
labels_classes = ...
    1 * (categories.camera == 1) + ...
    2 * (categories.book == 1) + ...
    3 * (categories.dvd == 1) + ...
    4 * (categories.health == 1) + ...
    5 * (categories.music == 1) + ...
    6 * (categories.software == 1);
                
%
% Sparar wordcount variabeln i en fil som hetet data.mat
%
save('../data.mat', 'wordcount', 'labels_sentiment', 'labels_classes');
