#!/bin/bash

if [ -f "data_process.py" ]; then
    echo "Running data_process.py"
    python data_process.py
else
    echo "File Not Found: data_process.py does not exist."
    exit 1
fi


if [ -f "model_fitting.py" ]; then
    echo "Running model_fitting.py"
    python model_fitting.py
else
    echo "File Not Found: model_fitting.py does not exist."
    exit 1
fi


if [ -f "plot_analysis.py" ]; then
    echo "Running plot_analysis.py with 'all' argument"
    python plot_analysis.py all
else
    echo "Error: plot_analysis.py does not exist."
    exit 1
fi


# 检查 report.tex 是否存在
if [ -f "report.tex" ]; then
    echo "Compiling report.tex"
    #!/bin/bash
    # Process input for other cases
    pdflatex report.tex
    bibtex report
    pdflatex report.tex
    pdflatex report.tex

    # Cleanup of extra files
    rm *.aux
    rm *.log
    rm *.bbl
    rm *.blg
    rm *.bcf
    rm *.run.xml

    echo "report.tex compiled successfully."

else
    echo "Error: report.tex does not exist."
    exit 1
fi


echo "All scripts executed successfully."