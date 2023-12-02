# MiniProject

**Project usage:** Clone this repository and run code/run_MiniProject.bash to run the whole project, the report "Mechanistic Models Outperform Phenomenological Models for Predicting Bacterial Growth" will be created in the results directory.

**Languages**: Python (version 3.8.18), , Bash (version 4.4.20(1))), LaTeX (pdfTeX version 3.14159265-2.6-1.40.20).

**Required Python packages**: 

- Numpy: Used for mathematical calculations and matrix representation
- Pandas: Used to read csv and data representations
- Lmfit: Due to the construction of fitted models
- Matplotlib: For visual analysis

**Project structure:**

The code directory contains Python scripts for data preparation as well as model fitting, plotting, and analysis. The bash script is used to run the entire project with one click. The data catalog contains csv files for bacterial growth data and corresponding metadata files :LogisticGrowthData.csv and LogisticMetaData.csv.

- **code:** Directory, python source code and bash source code
  - **figs:** A directory for storing pictures needed for latex
  - **mybibs.bib:** Citation of report
  - **report.tex:** lab report
  - **data_process.py:** -Used to process the initial data set, which can be preprocessed and divided into 285 sub-data sets stored in the result/subsets directory.
  - **mymodels.py:** Two linear models and two nonlinear models are defined.
  - **model_fitting.py:** Import the model fitting dataset in mymodels.py and output the results to the result/results directory.
  - **plot_analysis.py:** For visualization and analysis of results, visualize the results under result/results and store them in result/plots.
  - **run_MiniProject.bash:** The project code can be started and a pdf report generated.
- **data:** Directory to store the initial data set
  - LogisticGrowthData.csv: the dataset of courses
  - **LogisticMetaData.csv:** Introduction to the data set
- **result:** Directory to store the results
  - **plots:** It is used to store the results of visual analysis, that is, the results of model fitting
  - **subsets:** Subdata set
  - results: The analysis result of each subdataset corresponds to a behavior nan if the fit fails
