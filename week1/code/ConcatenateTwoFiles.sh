#!/bin/bash

# deal with exception or error 
if [ $# -ne 2 ]; then
    echo "Usage: $0 filepath1 filepath2"
    exit 1
fi

file1="$1"
file2="$2"

# check file whether exists
if [ ! -f "$file1" ]; then
    echo "file '$file1' not found"
    exit 1
fi

if [ ! -f "$file2" ]; then
    echo "file '$file2' not found"
    exit 1
fi


cat $1 > $3
cat $2 >> $3
echo "Merged File is"
cat $3
