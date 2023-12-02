import pandas as pd
import numpy as np
import sys
import os


from mymodels import Cubic, Quadratic, Gompertz, Logistic
  
def caculate_init_params(x, y):
    """
    For nonlinear models, initial parameters are obtained (can also be adjusted manually)
    """
    # Calculate the slope between two adjacent points
    slopes = np.diff(y) / np.diff(x)
    max_slope_index = np.argmax(slopes) #Index corresponding to the maximum slope
    # Find the maximum slope value
    N_0 = np.min(y)
    N_max = np.max(y)
    r_max = np.max(slopes)

    t_lag = x[max_slope_index] - y[max_slope_index] / r_max
    return N_0, N_max, r_max, t_lag

def deal_subset(subset, subsets_dir, results_dir):
  """
  load subdataset
  """
  filepath = f"{subsets_dir}/{subset}"
  save_path = f"{results_dir}/{subset}"
  data = pd.read_csv(filepath)
  """
  Dose model fitting and calculate AIC, AICc,BIC, R^2
  """
  x, log_y = data['Time'], data['logPopBio']
  x, log_y = np.array(x), np.array(log_y)
  x_vec = np.linspace(np.min(x), np.max(x), 1000)

  # init params
  N_0, N_max, r_max, t_lag = caculate_init_params(x, log_y)
  # print(N_0, N_max, r_max)
  models = [Cubic(), Quadratic(), Logistic(N_0, N_max, r_max), Gompertz(N_0, N_max, r_max,t_lag)]
  labels = ["Cubic", "Quadratic", "Logistic", "Gompertz"]

  results = []
  results.append({"x":x, "log_y":log_y}) 
  # plt.plot(x, log_y, '+', color='r' , markersize = 5, label = 'Data') # Original dot plot

  for i, model in enumerate(models):
    try:
      model.fit(x, log_y)
    except:
      print(f"{labels[i]}======failure fit ===>{filepath}")
      results.append({})
      # traceback.print_exc()
      continue
    log_y_vec = np.zeros(len(x_vec))
    residual_smooth = model.residuals(model.fitted.params, x_vec, log_y_vec)
    res = {  #Add fitted analysis results
      "AIC": model.aic,
      "AICc": model.aicc,
      "BIC": model.bic,
      "R^2": model.r_2,
      "x_vec":x_vec,
      "y_vec":residual_smooth + log_y_vec

    }
    # print(res['AIC'], res['AICc'], res['BIC'], res['R^2'])
    results.append(res)
    # plt.plot(res['x_vec'], res['y_vec'], 'red', linestyle = '--')

  # plt.show()

  
  # Save the fitted result data
  data = {
    "type": ["Data", "Cubic", 'Quadratic', 'Logistic', 'Gompertz'],
    "AIC": [],
    "AICc": [],
    "BIC": [],
    "R^2": [],
    "x_vec": [],
    "y_vec": [],
    "x":[],
    "log_y":[]
  }
  for res in results:
    data['AIC'].append(res.get("AIC", None))
    data['AICc'].append(res.get("AICc", None))
    data['BIC'].append(res.get("BIC", None))
    data['R^2'].append(res.get("R^2", None))
    data['x_vec'].append(res.get("x_vec", None))
    data['y_vec'].append(res.get("y_vec", None))
    data['x'].append(res.get("x", None))
    data['log_y'].append(res.get("log_y", None))
  data = pd.DataFrame(data)
  data.to_csv(save_path)



def main(args):
  subsets_dir = "../result/subsets"
  results_dir = "../result/results"
  if not os.path.exists(subsets_dir):
    os.mkdir(subsets_dir)
  if not os.path.exists(results_dir):
    os.mkdir(results_dir)


  for subset in os.listdir(subsets_dir): 
      deal_subset(subset, subsets_dir, results_dir)


if __name__=="__main__":
  main(sys.argv)




