### This script combines quality csvs into a one big one
###
### Ellyn Butler
### July 8, 2025 - September 18, 2025

import glob
import csv
import pandas as pd
from datetime import datetime

basedir = '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging'

files = glob.glob(basedir+'/surf/sub*/ses*/func/*_quality.csv')
df = pd.concat((pd.read_csv(f, header = 0) for f in files))

df.to_csv(basedir+'/tabulated/quality_'+datetime.today().strftime('%Y-%m-%d')+'.csv', index=False)