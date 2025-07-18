### This script combines the individual session csvs of amygdala connectivity
### into one master csv
###
### Ellyn Butler
### July 16, 2025

import glob
import csv
import pandas as pd
from datetime import datetime

basedir = '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging'

files = glob.glob(basedir+'/surfnet/sub*/ses*/*_surf_network_metrics.csv')
df = pd.concat((pd.read_csv(f, header = 0) for f in files))

df.to_csv(basedir+'/tabulated/surf_network_metrics_'+datetime.today().strftime('%Y-%m-%d')+'.csv', index=False)
