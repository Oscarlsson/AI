load('data_snowball_2000.mat')
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

figure
hold on
    plot(xData(labels_classes == 4), yData(labels_classes == 4),'.black')
    plot(xData(labels_classes == 6), yData(labels_classes == 6),'.m')
    plot(xData(labels_classes == 1), yData(labels_classes == 1),'.r')
    
    axis([-1 10 -2 10])
    legend('Health', 'Software', 'Camera')
    xlabel('v_1')
    ylabel('v_2')
    title('Projection of 2000 features onto principal components')
    %legend('Camera', 'Books', 'DVD', 'Health', 'Music', 'Software');
hold off

figure
hold on
    plot(xData(labels_classes == 2), yData(labels_classes == 2),'.g')
    plot(xData(labels_classes == 3), yData(labels_classes == 3),'.b')
    plot(xData(labels_classes == 5), yData(labels_classes == 5),'.c')
    
    axis([-1 10 -2 10])
    legend('Books', 'DVD', 'Music');
    %legend('Camera', 'Books', 'DVD', 'Health', 'Music', 'Software');
    title('Projection of 2000 features onto principal components')
    xlabel('v_1')
    ylabel('v_2')
hold off

figure
hold on
    plot(xData(labels_classes == 1), yData(labels_classes == 1),'.r')    
    plot(xData(labels_classes == 2), yData(labels_classes == 2),'.g')
    plot(xData(labels_classes == 3), yData(labels_classes == 3),'.b')
    plot(xData(labels_classes == 4), yData(labels_classes == 4),'.black')
    plot(xData(labels_classes == 5), yData(labels_classes == 5),'.c')
    plot(xData(labels_classes == 6), yData(labels_classes == 6),'.m')
    
    axis([-1 10 -2 10])
    legend('Camera', 'Books', 'DVD', 'Health', 'Music', 'Software');
    %legend('Camera', 'Books', 'DVD', 'Health', 'Music', 'Software');
    title('Projection of 2000 features onto principal components')
    xlabel('v_1')
    ylabel('v_2')
hold off

figure
hold on
    plot(xData(labels_classes == 3), yData(labels_classes == 3),'.b')
    plot(xData(labels_classes == 1), yData(labels_classes == 1),'.m')
    
    axis([-1 10 -2 10])
    title('Projection of 2000 features onto principal components')
    legend('DVD','Camera');
    %legend('Camera', 'Books', 'DVD', 'Health', 'Music', 'Software');
    xlabel('v_1')
    ylabel('v_2')
hold off