#!/bin/bash

# Detect whether the user has entered a file path
if [ "$#" -eq 0 ]; then
  printf "\nError: Needs a latex filename as input\n"
  exit 1
fi

# The file path entered is not a file that ends in tex
if [ "${1: -4}" != ".tex" ]; then
  printf "\nError: Need to input a template file that ends in a .tex file\n"
  exit 1
fi

# Get the filename without suffix
fbasename=$(echo "$1" | cut -f 1 -d '.')

pdflatex -halt-on-error -output-directory . -synctex=1 $1
bibtex $fbasename
pdflatex -halt-on-error -output-directory . -synctex=1 $1
pdflatex -halt-on-error -output-directory . -synctex=1 $1

if [ "$2" == "view" ]; then
  if [ -s "$fbasename.pdf" ]; then
    evince "$fbasename.pdf" &
  else
    echo "$fbasename.pdf is empty!"
  fi
fi

## Cleanup
rm *.aux
rm *.log
rm *.bbl
rm *.blg