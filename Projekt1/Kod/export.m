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
                    ,Ys(5,:),'music',Ys(6,:),'software',Ys(7,:)) ;
                
%
% Sparar wordcount variabeln i en fil som hetet data.mat
%
save('data.mat', 'wordcount', 'categories')
