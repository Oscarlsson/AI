#!/usr/bin/python 

import sys

def run():
        try: 
            f = open('filenames_final.txt') 
            file_names = f.read().split() 
            f.close() 
        except:
            print 'filenames_final.txt not found'
            sys.exit(1)
        ss = ["pos","camera","book","dvd","health","music","software"]
        Ys = [[None]*len(file_names) for k in range (0,len(ss))] 
        for i, word in enumerate(ss):
            for j , elem in enumerate(file_names):
                if word in elem:
                        Ys[i][j] = 1
                else:
                        Ys[i][j] = -1

        s = ""
        for i, elem in enumerate(ss):
             s = s + elem + "=" + str(Ys[i]) + ";\n"
        try: 
            f = open('Y.m', 'w')
            f.write(s)
            f.close()
        except:
            print "could not write Y.m"
run () 
