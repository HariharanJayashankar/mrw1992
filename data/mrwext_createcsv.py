import numpy as np
import pandas as pd
from linearmodels import PanelOLS
from statsmodels.iolib.summary2 import summary_col
import matplotlib.pyplot as plt
import os
import sys

os.chdir(sys.path[0])

data = pd.read_excel('pwt90.xlsx', sheet_name = 'Data').iloc[:, [0, 1, 3, 4, 6, 7, 9, 14, 21, 24]]

##Constructing variables
#grouping by country
data = data.sort_values(by = ['countrycode', 'year'])

#creating variables of interest
#gdp per worker
data['rgdpew'] = data['rgdpe']/data['emp']
data['const'] = 1

#pop growth by country
data['popgrowth'] = data.groupby('countrycode').pop.diff()/(data['pop'])

#investment
data['ck_1'] = data.groupby('countrycode').ck.shift(-1)
data['investment'] = data['ck_1']- (data['const'] - data['delta'])*data['ck']
data['i_y'] = data['investment']/data['rgdpe']

#TFP
data['g'] = data.groupby('countrycode').rtfpna.diff()/(data['rtfpna'])

#dropping NaN's
data.i_y = data.i_y.replace(0, np.nan)
data = data.dropna(subset = ['popgrowth', 'g', 'delta', 'i_y'])

#Constructing Logs
data['ly'] = np.log(data['rgdpe'])
data['lschool'] = np.log(data['hc'])
data['ls'] = np.log(data['i_y']) #unable to take log
data['lngd'] = np.log((data['popgrowth'] + data['g'] + data['delta']).replace(0, np.nan)) #unable to take log
data['ls_lngd'] = data['ls']
data['lsch_lngd'] = data['lschool'] - data['lngd']
data['t'] = data['year'] - data.groupby('countrycode')['year'].transform('first')
data['gt'] = data['g'] * data['t']

data.to_csv('panel_mrw.csv')