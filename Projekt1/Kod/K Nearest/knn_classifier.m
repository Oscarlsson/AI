function [ classification ] = knn_classifier( data1, data2, K, x )


c1distances = sort(arrayfun(@(i) norm(data1(i,:) - x), 1:size(data1,1))');
c2distances = sort(arrayfun(@(i) norm(data2(i,:) - x), 1:size(data2,1))');

classification = 0;
c1i = 1;
c2i = 1;
for i = 1:K
    if (c1distances(c1i) < c2distances(c2i))
        classification = classification + 1;
        c1i = c1i + 1;
    else
        classification = classification - 1;
        c2i = c2i + 1;
    end
end

if classification == 0
    classification = rand() - 0.5;
end

classification = sign(classification);

end

