### This script combines the individual session csvs of amygdala connectivity
### into one master csv
###
### Ellyn Butler
### August 17, 2025 - September 24, 2025

import glob
import csv
import pandas as pd
from datetime import datetime

basedir = '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging'

files = glob.glob(basedir+'/surfnet/sub*/ses*/*_surf_network_metrics_z1.csv')
df = pd.concat((pd.read_csv(f, header = 0) for f in files))

df.to_csv(basedir+'/tabulated/surf_network_metrics_z1_'+datetime.today().strftime('%Y-%m-%d')+'.csv', index=False)
