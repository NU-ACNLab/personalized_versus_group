### This script combines quality csvs into a one big one
###
### Ellyn Butler
### August 16, 2025

import glob
import csv
import pandas as pd
from datetime import datetime

basedir = '/projects/b1108/studies/personalized_versus_group/data/processed/neuroimaging/tabultaed'

files = glob.glob(basedir+'/sub*/ses*/func/*_quality.csv')
df = pd.concat((pd.read_csv(f, header = 0) for f in files))

df.to_csv(basedir+'/meanFD_'+datetime.today().strftime('%Y-%m-%d')+'.csv', index=False)