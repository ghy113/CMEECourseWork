#!/bin/sh
# Author: hg2423@ic.ac.uk
# Script: tabtocsv.sh
# Description: substitute the tabs in the files with commas
#
# Saves the output into a .csv file
# Arguments: 1 -> tab delimited file
# Date: Oct 2019

# add deal with expection or error
if [ $# -ne 1 ]; then
    echo "Usage: $0 filename.csv"
    exit 1
fi
# Check if the input file exists
if [ ! -f "$1" ]; then
    echo "Input file not found: $1"
    exit 1
fi

echo "Creating a comma delimited version of $1 ..."
cat $1 | tr -s "\t" "," >> $1.csv
echo "Done!"
exit