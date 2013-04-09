#!/usr/bin/python 
import sys
from collections import *

#All input files must specified from stdin 
#run for example: find rot_path_to_files | grep .txt$  | ./counter.py
#note the file english_word_stops.txt must be in the working directory 

s = "" 

try: 
    for line in sys.stdin: 
        f = open(line.strip('\n'))
        s = s + f.read() + " " 
        f.close()
except:
    print "failed to open input files"
    sys.exit(1)

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

output = ""

for elem in c.most_common(len(ss)):
     output = output + str(elem) + '\n'
try:
    f = open('output.txt','w')
    f.write(output)
    f.close() 
except:
    print "not allowed to write to output.txt"
    sys.exit(1)
