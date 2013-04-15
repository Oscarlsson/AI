#!/usr/bin/python 
import sys
from collections import *
from cmath import log

#All input files must specified from stdin 
#run for example: find root_path_to_files | grep .txt$  | ./counter.py
#note the file english_word_stops.txt must be in the working directory 

def run():
    filenames = "" # To be printed to filenames.txt in the end...

    print "Reading files..."
    try: 
        wordSet = set() # Set of all observed words
        cList = [] # One Counter object for each document
        for line in sys.stdin:
            f = open(line.strip('\n'))
            filenames = filenames + line
            
            c = count_words_tf_normalized(f.read()) # Counter
            cList.append(c)

            # Keep all seen words in a set,
            # used later only for indexing.
            wordSet = wordSet.union(c.keys())

            f.close()
    except:
        print "failed to open input files"
        raise
        sys.exit(1)

    nDocuments = len(cList)

    print "Creating dictionary..."
    d = dict()
    for i, word in enumerate(wordSet):  
        d[word] = i + 1 # Map each word to an index

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

    print "Creating output..."
    s = ""
    for c in cList:
        for word, tf in c.items():
            # Replace word with d[word] (index) in output:
            idf = log ( nDocuments / dWordDocOccurrences[word] ).real
            s = s + str(d[word]) + ':' + str(tf * idf) + ' '
            #idf = 1
            #s = s + "foo" + ':' + str(tf * idf) + ' '
        s = s + '\n'

    print "Printing output..."
    try:
        f = open('output.txt','w')
        f.write(s)
        f.close() 
    except:
        print "not allowed to write to output.txt"
        sys.exit(1)

    try:
        f = open('filenames.txt','w')
        f.write(filenames)
        f.close() 
    except:
        print "not allowed to write to output.txt"
        sys.exit(1)

# Input file contents
# Returns counts in range [0,1] for each word in each doc
def count_words_tf_normalized(s):
    try: 
        f = open("english_word_stops.txt")
        common_words = f.read().split()  
        f.close() 
    except:
        print "can't find file english_word_stops.txt"
        sys.exit(1) 

    rm = [str(x) for x in range (0,10)] + [',','.',':','&','-','(',')','$','/','"',';','!','?']

    s = filter(lambda x : not x in rm,s) 

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


# Input file contents
def count_words(s):
    try: 
        f = open("english_word_stops.txt")
        common_words = f.read().split()  
        f.close() 
    except:
        print "can't find file english_word_stops.txt"
        sys.exit(1) 

    rm = [str(x) for x in range (0,10)] + [',','.',':','&','-','(',')','$','/','"',';','!','?']

    s = filter(lambda x : not x in rm,s) 

    ss = s.lower().split() 

    c = Counter(ss)

    for elem in common_words:
        del c[elem]

    return c


run()
