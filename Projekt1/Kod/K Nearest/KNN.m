%%
%%
%% K - nearest neightbour classifier 
%%
%%

load('digits.mat');

%Data
%datap= [data(:,:,5) data(:, :, 8)]';

%Labels. All fives have label 1 and all 8 have label -1
%y = [ones(length(data(:,:,5)),1*1) ; ones(length(data(:,:,8)),1)*-1];


for i = 1:20

    mdl = ClassificationKNN.fit(datap,y, 'NumNeighbors', i);
    cvmdl = crossval(mdl);
    kloss = kfoldLoss(cvmdl);
    loss(i) = kloss;

end

plot(loss);
ylabel('error in %');
xlabel('k');
