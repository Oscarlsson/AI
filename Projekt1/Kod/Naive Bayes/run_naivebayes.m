function [ classifications ] = run_naivebayes( training_data, training_labels, test_data, nWords, useTfIdf )

%% Setup, data.

nDocuments = size(training_data,2);

%% Classes initialization
%nClasses = size(unique(training_labels),2);
nClasses  = max(training_labels);
%% NB start

% Setup Pc(c) =
%   = P(c) =
%   = (#docs with class c) / (#docs)
Pc = zeros(1,2);
for i = 1:nClasses
    Pc(i) = sum(training_labels == i) / length(training_labels);
end

laplace = 1;
% Setup Pwc(w, c) = P(w | c) = (#words=w in class c) / (#words in class c)
Pwc = laplace*ones(nWords, nClasses); % Note: ones. Not zeroes. Laplace smoothing.
for d = 1:nDocuments
    class = training_labels(d);
    for i = 1:length(training_data{d}.id)
        docWordId = training_data{d}.id(i);
		if useTfIdf
			docWordCount = training_data{d}.cnt(i);
		else
			docWordCount = 1;
		end
        Pwc(docWordId, class) = ...
            Pwc(docWordId, class) + docWordCount;
    end
end
% Normalize Pwc: Divide each element by the column sum.
% Due to Laplace smoothing, add +nWords to denominator.
% See http://nlp.stanford.edu/IR-book/html/htmledition/naive-bayes-text-classification-1.html
Pwc = Pwc ./ (ones(nWords, 1) * sum(Pwc) + nWords*laplace);


%% Generate classifications (output)
classifications = cellfun(@(x) nb_classify(x, Pc, Pwc), test_data);

end

