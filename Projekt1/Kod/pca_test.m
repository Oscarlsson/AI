load('data.mat')
training_data = wordcount;
nWords = max(cellfun(@(x) max(x.id), wordcount))
nTrainingDocuments = size(training_data, 2);
Xtraining = zeros(nTrainingDocuments, nWords);
for d = 1:nTrainingDocuments
    for i = 1:size(training_data{d}.id, 2)
        w = training_data{d}.id(i);
        tfidf = training_data{d}.cnt(i);
        Xtraining(d,w) = tfidf;
    end
end

[score, coeff] = princomp(Xtraining);
%%

xData = Xtraining * score(:,1);
yData = Xtraining * score(:,2);
zData = Xtraining * score(:,3);

% hold on
% plot(xData(labels_sentiment == 1), yData(labels_sentiment == 1),'.b')
% plot(xData(labels_sentiment == 2), yData(labels_sentiment == 2),'.r')
% hold off
% figure
hold on

plot(xData(labels_classes == 1), yData(labels_classes == 1),'.r')
plot(xData(labels_classes == 2), yData(labels_classes == 2),'.g')
plot(xData(labels_classes == 3), yData(labels_classes == 3),'.b')
plot(xData(labels_classes == 5), yData(labels_classes == 5),'.c')

% plot(xData(labels_classes == 4), yData(labels_classes == 4),'.black')
% plot(xData(labels_classes == 6), yData(labels_classes == 6),'.m')

hold off

% figure
% hold on
% plot3(xData(labels_classes == 1), yData(labels_classes == 1), zData(labels_classes==1), '.r')
% plot3(xData(labels_classes == 2), yData(labels_classes == 2), zData(labels_classes==2), '.g')
% hold off