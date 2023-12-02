
from lmfit import Minimizer, Parameters, report_fit
import numpy as np
"""
The defined model has two linear and two nonlinear:
linear:
  1. Cubic: ax^3 + bx^2 + cx + d
  2. Quadratic: ax^2 + bx + c
non-linear:
  2. Gompertz
  3. Logistic

"""


# linear model
class Quadratic:
  def __init__(self) -> None:
    #Create object for storing parameters
    self.params = Parameters()
    #Add parameters and initial values to it
    self.params.add('a', value = 1)
    self.params.add('b', value = 1)
    self.params.add('c', value = 1)
    
  def residuals(self, params, t, data):
    """Calculate cubic growth and subtract data"""
    
    #Get an ordered dictionary of parameter values
    v = params.valuesdict()
    
    #Cubic model
    model = v['a']*t**2 + v['b']*t + v['c']
    return model - data     #Return residuals
  

  def fit(self, x, y):
    #Create a Minimizer object
    minner = Minimizer(self.residuals, self.params, fcn_args=(x, y))
    #Perform the minimization
    self.fitted = minner.minimize()
    self.aic = self.fitted.aic
    self.bic = self.fitted.bic
    self.aicc = self.aic + (2 * 3 * (3 + 1)) / (len(x) - 3 - 1)
    self.r_2 = 1 - self.fitted.redchi



class Cubic:
  def __init__(self) -> None:
    #Create object for storing parameters
    self.params = Parameters()
    #Add parameters and initial values to it
    self.params.add('a', value = 1)
    self.params.add('b', value = 1)
    self.params.add('c', value = 1)
    self.params.add('d', value = 1)
  def residuals(self, params, t, data):
    """Calculate cubic growth and subtract data"""
    
    #Get an ordered dictionary of parameter values
    v = params.valuesdict()
    
    #Cubic model
    model = v['a']*t**3 + v['b']*t**2 + v['c']*t + v['d']
    return model - data     #Return residuals

  def fit(self, x, y):
    #Create a Minimizer object
    minner = Minimizer(self.residuals, self.params, fcn_args=(x, y))
    #Perform the minimization
    self.fitted = minner.minimize()
    self.aic = self.fitted.aic
    self.bic = self.fitted.bic
    self.aicc = self.aic + (2 * 4 * (4 + 1)) / (len(x) - 4 - 1)
    self.r_2 = 1 - self.fitted.redchi

  


# Gompertz
class Gompertz:
  def __init__(self, N_0, N_max, r_max, t_lag):
    #Create object for parameter storing
    self.params = Parameters()
    self.params.add('N_0', value = N_0)
    self.params.add('N_max', value = N_max)
    #Recall the value for growth rate obtained from a linear fit
    self.params.add('r_max', value = r_max)
    self.params.add('t_lag', t_lag)
    
  def residuals(self, params, t, data):
    '''Model a logistic growth and subtract data'''
    #Get an ordered dictionary of parameter values
    v = params.valuesdict()
    #Logistic model
    model = v['N_0'] + (v['N_max'] - v['N_0']) * np.exp(-np.exp(v['r_max'] * np.exp(1) * (v['t_lag'] - t) / \
                   ((v['N_max'] - v['N_0']) * np.log(10)) + 1))
    
    return model - data

  def fit(self, x, y):
    minner = Minimizer(self.residuals, self.params, fcn_args=(x, y))
    self.fitted = minner.minimize()
    self.aic = self.fitted.aic
    self.bic = self.fitted.bic
    self.aicc = self.aic + (2 * 4 * (4 + 1)) / (len(x) - 4 - 1)
    self.r_2 = 1 - self.fitted.redchi


class Logistic:
  def __init__(self, N_0, N_max, r_max) -> None:
    #Create object for parameter storing
    self.params = Parameters()
    self.params.add('N_0', value = N_0)
    self.params.add('N_max', value = N_max)
    #Recall the value for growth rate obtained from a linear fit
    self.params.add('r', value = r_max)
  def residuals(self, params, t, data):
    '''Model a logistic growth and subtract data'''
    #Get an ordered dictionary of parameter values
    v = params.valuesdict()
    #Logistic model
    model = v['N_0'] * v['N_max'] * np.exp(v['r']*t) / \
    (v['N_max'] + v['N_0'] * ( np.exp(v['r']*t) - 1 ))
    # model = np.log(model)
    return model - data
  
  def fit(self, x, y):
    #Create a Minimizer object
    minner = Minimizer(self.residuals, self.params, fcn_args=(x, y))#Plug in the logged data.
    #Perform the minimization
    self.fitted = minner.minimize(method = 'leastsq')
    self.aic = self.fitted.aic
    self.bic = self.fitted.bic
    self.aicc = self.aic + (2 * 3 * (3 + 1)) / (len(x) - 3 - 1)
    self.r_2 = 1 - self.fitted.redchi


def test_model():
  from lmfit import Minimizer, Parameters, report_fit
  import numpy as np
  import matplotlib.pylab as plt
  t = np.arange(0, 24, 2)
  N = np.array([32500, 33000, 38000, 105000, 445000, 1430000, 3020000, 4720000, 5670000, 5870000, 5930000, 5940000])
  np.random.seed(1234) #Set random seed for reproducibility
  N_rand = N*(1 + np.random.normal(scale = 0.1, size = len(N))) #Add some error to data
  plt.plot(t, N_rand, 'r+', markersize = 15, markeredgewidth = 2, label = 'Data')
  plt.xlabel('t', fontsize = 20)
  plt.ylabel(r'$N$', fontsize = 20)
  plt.ticklabel_format(style='scientific', scilimits=[0,3])
  plt.yscale('log')
  # model = Cubic()
  # model = Logistic(N_rand[0], N_rand[-1], 0.62)
  model = Gompertz(np.log(N_rand)[0], np.log(N_rand)[-1], 0.62)
  model.fit(t, np.log(N_rand))
  report_fit(model.fitted)
  print(model.aic, model.aicc, model.bic, model.r_2)

if __name__=="__main__":
  test_model()