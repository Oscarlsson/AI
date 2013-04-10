#!/usr/bin/python 
import sys
from collections import *

#All input files must specified from stdin 
#run for example: find root_path_to_files | grep .txt$  | ./counter.py
#note the file english_word_stops.txt must be in the working directory 

def run():
	s = "" 

	try: 
	    for line in sys.stdin:
		f = open(line.strip('\n'))
		output =  count_words(f.read());
		label = get_label(line)
		s = s + "------New Doc------- " + label + " ---------\n" +  output 
		f.close()
	except:
	    print "failed to open input files"
	    raise
	    sys.exit(1)

	try:
	    f = open('output.txt','w')
	    f.write(s)
	    f.close() 
	except:
	    print "not allowed to write to output.txt"
	    sys.exit(1)


#Returns label 1/-1 depending on if given path has "neg" or "pos"
def get_label(line):
	if "pos" in line:
		return "1"
	else:
		return "-1"	


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

	output = ""

	for elem in c.most_common(len(ss)):
	     output = output + str(elem) + '\n'

	return output

run()
