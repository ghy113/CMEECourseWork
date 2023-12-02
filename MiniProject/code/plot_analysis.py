import pandas as pd
from lmfit import Minimizer, Parameters, report_fit
import numpy as np
import matplotlib.pylab as plt
import sys
import os



def np_from_string(string_data):
  """
  a string like "[1 2 3]"  convert to numpy
  """
  if type(string_data) == str:
    # Remove square brackets and use Spaces to separate numbers
    cleaned_string = string_data.strip('[]')
    numbers = cleaned_string.split()
    # Converts a string array to a NumPy array
    return np.fromstring(' '.join(numbers), sep=' ')
  else:
    return []


def fit_count_analysis():
  """
  Each of the five models can converge on how many data sets, and return the data set that can converge
  """
  models_name = ["Cubic", "Quadratic", "Logistic", "Gompertz"]
  result = {
    "Cubic":set([i for i in range(285)]),
    "Quadratic":set([i for i in range(285)]),
    "Logistic":set([i for i in range(285)]),
    "Gompertz":set([i for i in range(285)])
  }
  all_fitted_subsets = []
  best_aicc_count = {
    "Cubic":set(),
    "Quadratic":set(),
    "Logistic":set(),
    "Gompertz":set()
  }
  best_bic_count = {
    "Cubic":set(),
    "Quadratic":set(),
    "Logistic":set(),
    "Gompertz":set()
  }

  for i in range(285):
    subset = f"../result/results/subset{i}.csv"
    data = pd.read_csv(subset)
    # Check for na in rows [1, 4] of column 2 (index 1)
    na_in_col = data.iloc[[1,2,3,4], 2].isna()
    na_rows = na_in_col.loc[na_in_col].index
    if len(na_rows) != 0:
      for index in na_rows:
        result[models_name[index - 1]].remove(i)
    else:
      all_fitted_subsets.append(i)
      aic_min_index = data['AICc'].idxmin()
      name = data.iloc[aic_min_index]['type']
      best_aicc_count[name].add(i)

      bic_min_index = data["BIC"].idxmin()
      name = data.iloc[bic_min_index]['type']
      best_bic_count[name].add(i)

  print("all fit:", len(all_fitted_subsets), all_fitted_subsets)
  #Fit condition
  plt.figure(figsize=(8, 6))
  counts = [len(result[label]) for label in models_name]
  bars = plt.bar(models_name, counts, color=["red", 'blue', 'yellow', 'orange'], label=models_name)
  plt.ylabel("Count")
  plt.ylim((0, 350))
  plt.legend()
  for bar in bars:
      yval = bar.get_height()
      plt.text(bar.get_x() + bar.get_width()/2, yval, int(yval), va='bottom', ha='center')
  plt.savefig("../result/plots/overview_count.png")
  plt.show()
  # Best fit case AICc
  # print(result["Cubic"])
  print("Best Aicc:")
  for key, value in best_aicc_count.items():
    print(key, len(value))
  print("Best Bic:")
  for key, value in best_bic_count.items():
    print(key, len(value))
  
  #Plot
  x = np.arange(4)
  width = 0.35
  aic_counts = [len(best_aicc_count[name]) for name in models_name]
  bic_counts = [len(best_bic_count[name]) for name in models_name]
  fig, ax = plt.subplots()
  ax.bar(x - width/2, aic_counts, width, label='AICc')
  ax.bar(x + width/2, bic_counts, width, label='BIC')
  # Add text labels, titles, and custom X-axis scale labels
  ax.set_ylabel('Count')
  ax.set_xticks(x)
  ax.set_xticklabels(models_name)
  ax.legend()
  plt.savefig("../result/plots/best_aicc_bic_count.png")
  plt.show()
  return all_fitted_subsets

def fit_subset_plot(subsets:list, isshow=False):
  for subset in subsets:
    subsetid = subset
    subset = f"../result/results/{subset}.csv"
    result = pd.read_csv(subset)
    line_colors = ['r', 'g', 'b', 'y']
    plt.cla()
    for index, row in result.iterrows():
      if index == 0: #raw data
        plt.plot(np_from_string(row['x']), np_from_string(row['log_y']), '+',color='r' , markersize = 5, label = row['type']) # 预测的点图
      else:
        plt.plot(np_from_string(row['x_vec']), np_from_string(row['y_vec']), line_colors[index - 1], linestyle = '--', label=row['type']) # 预测的线图
    plt.legend(fontsize = 10)
    plt.xlabel('Time', fontsize = 10)
    plt.ylabel('PipBio', fontsize = 10)
    
    plt.savefig(f"../result/plots/{subsetid}.png")
    if isshow:
      plt.show()


def fit_score_analysis(all_fit_sets, score_type="AICc"):
  labels = ["Cubic", "Quadratic", "Logistic", "Gompertz"]
  data = [[], [], [], []]
  for dataset in all_fit_sets:
    dataset = f"../result/results/subset{dataset}.csv"
    res = pd.read_csv(dataset)
    if(not pd.isna(res[score_type].iloc[1])):
      data[0].append(res[score_type].iloc[1])
    if(not pd.isna(res[score_type].iloc[2])):
      data[1].append(res[score_type].iloc[2])
    if(not pd.isna(res[score_type].iloc[3])):
      data[2].append(res[score_type].iloc[3])
    if(not pd.isna(res[score_type].iloc[4])):
      data[3].append(res[score_type].iloc[4])

  colors = ['red', 'blue', 'yellow', 'pink']
  print(f"the average of {score_type}:")
  print(labels)
  print([sum(item)/len(item) for item in data])
  # Create the boxplot with colors
  box = plt.boxplot(data,vert=True, showfliers=True, patch_artist=True, labels=labels)  # patch_artist=True to fill with color

  # Accessing the boxes using the axes object returned by the boxplot
  for boxpatch, color in zip(box['boxes'], colors):
      boxpatch.set_facecolor(color)
  plt.ylabel(f"{score_type}")
  if score_type == 'AICc':
    plt.savefig("../result/plots/overview_aicc")
  elif score_type == 'R^2':
    plt.savefig("../result/plots/overview_r2")
  else:
    plt.savefig(f"../result/plots/overview_{score_type}")
  plt.show()


def main(args):
  if not os.path.exists("../result/plots"):
    os.mkdir("../result/plots")
  if len(args) == 2:
    if args[1].upper() == 'ALL':
      print("Model fitting using the data set: subsets0~284")
      fit_subset_plot([f"subset{i}" for i in range(285)])
    else:
      print("Model fitting using the data set: subsets2")
      subset = args[2]
      fit_subset_plot([subset])
  else:
    print("Model fitting using the default data set: subsets1, subset100")
    fit_subset_plot(["subset1", "subset100"])
    
  all_fitted_subsets = fit_count_analysis()
  fit_score_analysis(all_fitted_subsets, "AICc") # aicc, bic, r2
  fit_score_analysis(all_fitted_subsets, "R^2") # AICc, BIC, R_2




if __name__=="__main__":
  main(sys.argv)