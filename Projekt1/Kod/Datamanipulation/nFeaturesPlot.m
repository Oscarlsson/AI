% This script shows the number of documents that each feature is present in
% for the two methods of Unigram and Bigram

dpf_unigram = importdata('docs_per_feature_unigram');
dpf_bigram = importdata('docs_per_feature_bigram');

maxUnigram = max(dpf_unigram(:,1));
maxBigram = max(dpf_bigram(:,1));

hold on;
    plot(dpf_unigram(:,2), dpf_unigram(:,1), 'b');
    plot(dpf_bigram(:,2), dpf_bigram(:,1), 'r', 'LineWidth', 2.5);
    legend(...
        sprintf('Unigram (max %d)', maxUnigram),...
        sprintf('Bigram (max %d)', maxBigram)...
    );
    axis([0 2000 0 500]);
    xlabel('Feature/word id')
    ylabel('# documents')
    title('Number of unique documents per feature')
hold off;