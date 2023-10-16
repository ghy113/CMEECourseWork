#!/bin/bash
# Check if the correct number of arguments is provided
if [ $# -ne 1 ]; then
    echo "Usage: $0 filename.csv"
    exit 1
fi
# Check if the input file exists
if [ ! -f "$1" ]; then
    echo "Input file not found: $1"
    exit 1
fi

# Define the output file name
output_file="${1%.csv}_space.txt"

# replace , to space
sed 's/,/ /g' "$1" > "$output_file"

echo "Conversion complete. Saved as $output_file"