# -*- coding: utf-8 -*-
def sim_normal(nums, mean = 600, sd = 30):
    import numpy as np
    import numpy.random as nr
    for n in nums:
        dist = nr.normal(loc = mean, scale = sd, size = n)
        titl = 'Normal distribution with ' + str(n) + ' values'
        print('Summary for ' + str(n) + ' samples')
        print(dist_summary(dist, titl))   
        print('Emperical 95% CIs')
        print(np.percentile(dist, [2.5, 97.5]))
        print(' ')
    return('Done!')

def sim_poisson(nums, mean = 600):
    import numpy as np
    import numpy.random as nr
    for n in nums:
        dist = nr.poisson(lam = mean, size = n)
        titl = 'Poisson distribution with ' + str(n) + ' values'
        print(dist_summary(dist, titl))    
        print('Emperical 95% CIs')
        print(np.percentile(dist, [2.5, 97.5]))
        print(' ')
    return('Done!')

def dist_summary(dist, names = 'dist_name'):
    import pandas as pd
    import matplotlib.pyplot as plt
    ser = pd.Series(dist)
    fig = plt.figure(1, figsize=(9, 6))
    ax = fig.gca()
    ser.hist(ax = ax, bins = 120)
    ax.set_title('Frequency distribution of ' + names)
    ax.set_ylabel('Frequency')
    plt.show()
    return(ser.describe())
    
def gen_profits(num):  
    import numpy.random as nr
    unif = nr.uniform(size = num)
    out = [5 if x < 0.3 else (3.5 if x < 0.6 else 4) for x in unif]
    return(out)

def gen_tips(num):  
    import numpy.random as nr
    unif = nr.uniform(size = num)
    out = [0 if x < 0.5 else (0.25 if x < 0.7 
      else (1.0 if x < 0.9 else 2.0)) for x in unif]
    return(out)

def sim_lemonade(num, mean = 600, sd = 30, pois = False):
  ## Simulate the profits and tips for
  ## a lemonade stand.
  import numpy.random as nr
  
  ## number of customer arrivals
  if pois:
    arrivals = nr.poisson(lam = mean, size = num)
  else:
   arrivals = nr.normal(loc = mean, scale = sd, size = num) 

  print(dist_summary(arrivals, 'customer arrivals per day'))
  
  ## Compute distibution of average profit per arrival
  proft = gen_profits(num)
  print(dist_summary(proft, 'profit per arrival'))
  
  ## Total profits are profit per arrival 
  ## times number of arrivals.
  total_profit = arrivals * proft 
  print(dist_summary(total_profit, 'total profit per day'))
  
  ## Compute distribution of average tips per arrival
  tps = gen_tips(num)
  print(dist_summary(tps, 'tips per arrival'))
  
  ## Compute average tips per day
  total_tips = arrivals * tps
  print(dist_summary(total_tips, 'total tips per day'))
  
  ## Compute total profits plus total tips.
  total_take = total_profit + total_tips 
  return(dist_summary(total_take, 'total net per day'))



