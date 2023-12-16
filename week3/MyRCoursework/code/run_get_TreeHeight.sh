# !/bin/bash

# Run R test
Rscript get_TreeHeight.R ../data/trees.csv

# Run python test
python3 get_TreeHeight.py ../data/trees.csv

# Echo the completion message
echo "Script completed. Check results directory for output."
