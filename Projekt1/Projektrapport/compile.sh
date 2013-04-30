#!/bin/bash
#Compile entire rapport.

pdflatex main.tex
bibtex main.aux
pdflatex main.tex
pdflatex main.tex
echo "Rapport has been compiled."
exit 0
