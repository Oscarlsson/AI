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
        j = 0 
        i = 0
        Ys = [[None]*len(file_names) for k in range (0,len(ss))] 
        for word in ss:
            j = 0 
            for elem in file_names:
                if word in elem:
                        Ys[i][j] = 1
                else:
                        Ys[i][j] = -1
                j = j + 1
            i = i + 1 

        s = ""
        i = 0 
        for elem in ss:
             s = s + elem + "=" + str(Ys[i]) + "\n"
             i = i + 1
        try: 
            f = open('Y.m', 'w')
            f.write(s)
            f.close()
        except:
            print "could not write Y.m"

run () 
