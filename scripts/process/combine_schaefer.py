### This script combines the individual session csvs of 
### Schaefer ROIs into one master csv
###
### Ellyn Butler
### December 24, 2025

import glob
import csv
import pandas as pd
import os
from datetime import datetime

basedir = '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging'

files = glob.glob(basedir+'/surfnet/sub*/ses*/*_schaefer_400.csv')
df = pd.concat((pd.read_csv(f, header = 0) for f in files))

df.to_csv(basedir+'/tabulated/schaefer_400_'+datetime.today().strftime('%Y-%m-%d')+'.csv', index=False)
