function [ errors, timings ] = indomain_task( algorithms, data, labels_sentiment, labels_classes, nWords )

nrAlgorithms = size(algorithms,2);

errors = zeros(1,nrAlgorithms);
timings = zeros(1,nrAlgorithms);
nDomainLabels = max(labels_classes);

for algoIndex = 1:nrAlgorithms

    errorAlg = zeros(1,nDomainLabels);
    timingAlg = zeros(1,nDomainLabels);
    for i = 1:nDomainLabels
       category_data = data(labels_classes == i);
       category_labels = labels_sentiment(labels_classes == i);
       [error, timing] = test_algorithm( algorithms{algoIndex}, category_data, category_labels, category_data, category_labels, 1, nWords );

       errorAlg(i) = error;
       timingAlg(i) = timing;
    end

    %Average over all labels in-domain
    errors(algoIndex) = mean(errorAlg);
    timings(algoIndex) = mean(timingAlg);
end


end

