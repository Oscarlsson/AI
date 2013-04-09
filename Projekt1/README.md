# Project 1: "Sentimentalizer"

This project concerns very important topics, which are used in all AI areas, not only NLP:
Machine learning of statistical classifiers, in this case applied to the task of categorizing documents;
Statistical analysis of experiments, such as comparing measurements of classification accuracy.
You will implement some basic classifiers yourself (Naive Bayes and the Perceptron), and use some existing libraries that implement other algorithms, such as k-nearest-neighbors, support vector machines, decision trees, maximum entropty classifiers, or probabilistic topic models.

## The data
You will use the following data:
The following zip file contains a subset of Mark Drezde's collection of Amazon customer reviews, nicely processed by Richard Johansson:
amazon-balanced-6cats.zip
The review collection is divided into six topic subdirectories: Books, Camera, DVD, Health, Music, and Software. The documents in each topic directory is then in turn divided into positive and negative subdirectories.
If you want to do more than the basic tasks, you might want to use the following datasets:
The original collection of customer reviews in 25 categories, collected from Amazon.com by Mark Dredze:
http://www.cs.jhu.edu/~mdredze/datasets/sentiment/
The file you want is called unprocessed.tar.gz. Note: The file is really large (1.5GB), and even larger (5GB) when you have decompressed it!
Movie review data collected from IMBD.com by Bo Pang and Lillian Lee:
http://www.cs.cornell.edu/people/pabo/movie-review-data/
The file you want is the "sentiment polarity dataset v2.0" (the actual file is called review_polarity.tar.gz). You might also be interested in the "pool of 27886 unprocessed html files" too (file polarity_html.zip).
The Reuters-21578 dataset, which contains 21578 news documents was published by Reuters in 1987. Each document is categorized in lots of different ways. It can be downloaded from here:
http://www.daviddlewis.com/resources/testcollections/reuters21578/

##Different classification algorithms
You should at least implement the following machine learning algorithms:
Naive Bayes
Perceptron
Averaged Perceptron (described below)
You should also test at least one other machine learning algorithm, such as such as k-nearest-neighbors, support vector machines, decision trees, maximum entropty classifiers, or probabilistic topic models.

##Classification tasks
The Amazon data can be used for at least three different kinds of classification tasks:
Text categorization: Categorize review documents into their six categories: Books, DVDs, Cameras, Music, Health, and Software.
In-domain sentiment analysis: For each review category, train and classify review documents as positive or negative. Which categories are easiest resp. hardest to classify?
Out-of-domain sentiment analysis: Make training and test sets for two different topics (such as Books and Cameras). Test how a classifier trained on book reviews performs on the camera test set and vice versa.
For each of these tasks you should use your implemented classifiers; Naive Bayes, Perceptron and Averaged Perceptron. You should also use as at least one external library of your own chosing.
To get more accurate evaluations, you should use N-fold cross-validation for each task. Also, think about how you want to split your data to be sure that you don't get any strange biases.

##Suggested reading
Here are some academic papers that describe these kinds of classification problems, and their associated algorithms.
Fabrizio Sebastiani: Machine learning in automated text categorization. ACM Computing Surveys, 34(1):1–47, 2002.
Bo Pang, Lillian Lee, and Shivakumar Vaithyanathan: Thumbs up? Sentiment Classification using Machine Learning Techniques. In Proceedings of the 2002 Conference on Empirical Methods in Natural Language Processing (EMNLP 2002).
Thorsten Joachims: Text Categorization with Support Vector Machines: Learning with Many Relevant Features. Technical report from the University of Dortmund, 1997.
John Blitzer, Mark Dredze, and Fernando Pereira: Biographies, Bollywood, Boom-boxes and Blenders: Domain Adaptation for Sentiment Classification. In Proceedings of the 45th Annual Meeting of the Association of Computational Linguistics (ACL 2007).
David M. Blei: Probabilistic Topic Models. Communications of the ACM, 55(4):77–84, 2012.
There are also references in chapter 22 of the textbook. Look at the "biographical and historical notes" in the end of the chapter.

##Statistical tests and manual analysis
On each of the tasks you perform you should evaluate the accuracy of your different classifiers. Compute confidence intervals and carry out McNemar tests to see whether the differences are statistically significant.
Print a few documents that are misclassified by the best of the classifiers. Try to see if you have any idea why they were difficult to classify.
What is the relation between the size of the training set and the performance of the classifier on the test set? Train classifiers on subsets of the training set and evaluate them all on the same test set. Report the result in a plot or a table.

##Additional tasks
There are several ways you can extend your work, here are some suggestions:
Try different feature sets, and see if you can get better results.
Some categories are closer to each other than others. Is there a correlation between out-of-domain sentiment analysis of A and B, and to categorize A texts as B?
Try the IMDB movie review dataset or the Reuters-21578 dataset, and see how well you perform there.
The original Amazon dataset contains large unprocessed files. Try to use them in some way to get better results.
Implement (and try) some more learning methods.
Something else of your choosing – but check with your supervisor first!

##Averaged perceptron
The perceptron learning algorithm has the drawback that its end result is biased toward the final examples it saw. A solution proposed by Freund and Schapire (1999) and also used by Collins (2002) is to return the average weight vector rather than the final weight vector.
It's a bit impractical to keep all versions of the weight vector in memory, and then taking the average at the end. Instead, we can build the average vector incrementally, updating it simultaneously while we're building the usual weight vector.
Then the perceptron algorithm will be something like this:
1. Let N = number of iterations through training set,
   and T = the size of the training set.

2. Initialize the weight vector w to all zeros,
   and the average weight vector wa to all zeros,
   and the counter c to 0.

3. Iterate N times through the training set:
    • For each x, y in the training set:
        a) Let guess = classify(x) using our current w
        b) If y is not equal to guess:
            • w := w + f(x, y) - f(x, guess)
            • average_weight := (N*T - c) / (N*T)
            • wa := wa + average_weight * (f(x, y) - f(x, guess))
        c) Increase the counter c

4. Return wa
