import pandas as pd
import scipy as sc
import numpy as np
import matplotlib.pylab as pl
import seaborn as sns # You might need to install this (e.g., pip install seaborn)
import os
"""
data loading
"""
data = pd.read_csv("../data/LogisticGrowthData.csv")
print("Loaded {} columns.".format(len(data.columns.values)))
print(data.columns.values)
readcsv = pd.read_csv("../data/LogisticGrowthMetaData.csv")
head = data.head()
print(readcsv)
print(head)

print(data.PopBio_units.unique()) #units of the response variable 

print(data.Time_units.unique()) #units of the independent variable 


"""
Deals with missing, and other problematic data values.
"""
# Remove non-digits from the Time column
data['Time'] = pd.to_numeric(data['Time'], errors='coerce')
data = data.dropna(subset=['Time'])
# Keep only positive values
data = data[data['Time'] > 0]
# Keep PopBio>0 values (delete missing values and values less than or equal to 0)
data['PopBio'] = pd.to_numeric(data['PopBio'], errors='coerce')
data = data.dropna(subset=['PopBio'])
data = data[data['PopBio'] > 0]

"""
Add a new column to Log the number of bacteria
"""
data['logPopBio'] = data['PopBio'].apply(lambda x: np.log(x))

"""
Creates unique ids
"""

data.insert(0, "ID", data.Species + "_" + data.Temp.map(str) + "_" + data.Medium + "_" + data.Citation) 
print("The number of data sets is:", len(data.ID.unique())) # units of the independent variable 

print("Is there a nan:", data.isna().any().any())
"""
It is divided into different data sets according to different ids
"""
if not os.path.exists("../result/subsets"):
  os.mkdir("../result/subsets")

for i, item in enumerate(data.ID.unique()):
  data_subset = data[data['ID'] == item]
  data_subset.to_csv(f"../result/subsets/subset{i}.csv")


