% Temporary quick test...
nCorrect = 0;
nWrong = 0;
results = [];
for testId = 1:length(wordcount)
    realClass = classes(testId);
    guessedClass = NaiveBayesImpl(wordcount{testId}, Pc, Pwc);
    results = [results; realClass, guessedClass];
    if (realClass == guessedClass)
        nCorrect = nCorrect + 1;
    else
        nWrong = nWrong + 1;
    end
end