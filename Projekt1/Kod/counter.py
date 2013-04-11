#!/usr/bin/python 
import sys
from collections import *

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
            
            c = count_words(f.read()) # Counter
            cList.append(c)
            wordSet = wordSet.union(c.keys())

            f.close()
    except:
        print "failed to open input files"
        raise
        sys.exit(1)

    print "Creating dictionary..."
    d = dict()
    for i, word in enumerate(wordSet):  
        d[word] = i # Map each word to an index

    print "Creating output..."
    s = ""
    for c in cList:
        for word, count in c.items():
            # Replace word with d[word] (index) in output:
            s = s + str(d[word]) + ':' + str(count) + ' '
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

#Input filename
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

    return c;

run()
