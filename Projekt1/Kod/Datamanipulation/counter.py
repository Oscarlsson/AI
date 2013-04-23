#!/usr/bin/python 
import sys
from collections import *
from cmath import log
import os 
import subprocess
import argparse 

# Run the file with the command: python counter.py -k <nr of words> 
# Alt. arguments is --snowball and --feature <bin/big>. Tfidf is default.


def run(snowballBool, kInt, feature):
    filenames = "" # To be printed to filenames.txt in the end...

    k = kInt # Number of words to output. Total: ~58 000.

    # Get files
    s = subprocess.Popen(["find", "../../Data/amazon-balanced-6cats/"], stdout=subprocess.PIPE) 
    s2 = subprocess.Popen(["grep", ".txt$"], stdin=s.stdout, stdout=subprocess.PIPE) 
    listoffiles = s2.stdout.read().split('\n')
    # delete last empty elem
    del listoffiles[-1]

    print "Reading files..."
    try: 
        cList = [] # One Counter object for each document
        for line in listoffiles:
            f = open(line)
            filenames = filenames + line
           
            c = count_words_tf_normalized(f.read(), snowballBool) # Counter
            cList.append(c)

            # Keep all seen words in a set,
            # used later only for indexing.

            f.close()
    except:
        print "failed to open input files"
        raise
        sys.exit(1)

    nDocuments = len(cList)

    print "Calculating word occurrences for the idf-term..."
    # Number of documents that each word occurs in.
    dWordDocOccurrences = dict()
    # Initialize dWordDocOccurrences:
    for c in cList:
        for word, count in c.items():
            if word in dWordDocOccurrences:
                dWordDocOccurrences[word] += 1
            else:
                dWordDocOccurrences[word] = 1

    print "Calculating tf-idf sum per word..."
    # For each Counter in cList, the tf-value is replaced with tf*idf
    wordIdfSum = Counter()
    for c in cList:
        for word, tf in c.items():
            idf = log ( nDocuments / dWordDocOccurrences[word] ).real
            try:
                wordIdfSum[word] += tf * idf
            except:
                wordIdfSum[word] = tf * idf

            if feature == "bin": 
                c[word] = 1
            else:
                c[word] = tf*idf

    print "Filtering words on tf-idf sum AND creating dictionary..."
    d = dict()
    for i, (word, tfidfsum) in enumerate(wordIdfSum.most_common(k)):
        d[word] = i

    # Expects: cList, d
    print "Printing output..."
    try:
        f = open('output.txt','w')
        for c in cList:
            for word, cnt in c.items():
                try:
                    f.write(str(d[word]) + ":"+ str(cnt) + " ")
                except:
                    pass
            f.write('\n')
        f.close() 
    except:
        print "not allowed to write to output.txt"
        raise
        sys.exit(1)

    try:
        f = open('filenames.txt','w')
        f.write(filenames)
        f.close() 
    except:
        print "not allowed to write to output.txt"
        sys.exit(1)

    try:
        f = open('vocabulary.txt','w')
        for word, count in d.items():
            f.write(str(count) + '\t' + word + '\n')
        f.close() 
    except:
        print "not allowed to write to output.txt"
        sys.exit(1)
    
    print "Calling export.m with octave.. "
#    output = subprocess.Popen(["octave","export.m"], stdout=subprocess.PIPE)
    # Consider using subprocess but make it wait
    # on the process before terminating.
    os.system("octave export.m")
    print "Done."

# Input file contents
# Returns counts in range [0,1] for each word in each doc
def count_words_tf_normalized(s, snowballBool):
    try: 
        f = open("english_word_stops.txt")
        common_words = f.read().split()  
        f.close() 
    except:
        print "can't find file english_word_stops.txt"
        sys.exit(1) 


    # Filter unwanted chars
    rm = [str(x) for x in range (0,10)] + [',','.',':','&','-','(',')','$','/','"',';','!','?']

    s = filter(lambda x : not x in rm,s) 
    
    #
    # Snowball! This is only run if argument
    # --snowball is set.
    # This is ugly because we create a stemmer-object each iteration. 
    #
    if snowballBool:
        import Stemmer
        stemmer = Stemmer.Stemmer('english')
        changedlist = map(stemmer.stemWord,s.split())
        s = ' '.join(changedlist)

    ss = s.lower().split() 

    c = Counter(ss)
    
    for elem in common_words:
        del c[elem]

    mostCommon = c.most_common(1)
    if not mostCommon:
        return c

    # mostCommonFreq = the count of the most common word
    mostCommonFreq = float(mostCommon[0][1])

    cOutput = Counter()

    # Normalized frequency tf(t, d) \in [0,1]
    for word, count in c.items():
        cOutput[word] = count / mostCommonFreq

    return cOutput

if __name__=="__main__":
   parser = argparse.ArgumentParser()
   
   parser.add_argument('--snowball', help='Run with snowball', action='store_true')
   parser.add_argument('--feature', help='Choose feature',  action='store', choices=['bin','big', 'tfidf'], default='tfidf')
   parser.add_argument('-k', help='Choose a big-ass-K. Must be an int.', type=int, required=True)
   
   args = vars(parser.parse_args())
   snowballBool = args['snowball']
   kInt = args['k']
   feature = args['feature']
  
   run(snowballBool, kInt, feature)


