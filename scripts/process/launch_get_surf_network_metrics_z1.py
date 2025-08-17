### This script generates submission obtaining personalized network metrics
###
### Ellyn Butler
### August 17, 2025

import os
import shutil
import re
import numpy as np
import glob
import nibabel as nib
import pandas as pd

basedir = '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/'
launchdir = basedir + 'launch/surfnet/'
indir = basedir + 'surf/'
outdir = basedir + 'surfnet/'
tabdir = basedir + 'tabulated/'

if not os.path.exists(launchdir):
    os.mkdir(launchdir)

if not os.path.exists(outdir):
    os.mkdir(outdir)

# Get the subject directories for the 387 subjects in the prior
df = pd.read_csv(tabdir + 'prior_subjects.csv')
subdirs = df['subid'].apply(lambda x: os.path.join(indir + 'sub-' + str(x)))
subdirs = subdirs.to_list()

for subdir in subdirs:
    sub = subdir.split('/')[9]
    if not os.path.exists(outdir+sub):
        os.mkdir(outdir+sub)
    sub_bold_imgs = glob.glob(indir+sub+'/ses-1/func/*_task-chatroom_run-01_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    subid = sub.split('-')[1]
    ses = 'ses-1'
    sesid = 1
    if not os.path.exists(outdir+sub+'/ses-1'):
        os.mkdir(outdir+sub+'/ses-1')
    if not os.path.exists(outdir + 'sub-' + subid + '/ses-1/sub-' +
                            subid + '_ses-1_surf_network_metrics_z1.csv'):
        cmd = ['Rscript /projects/b1108/projects/personalized_versus_group/scripts/process/get_surf_network_metrics_z1.R -s', subid, '-e 1']
        get_surf_network_metrics_script = launchdir+sub+'_'+ses+'_get_surf_network_metrics_z1_run.sh'
        os.system('cat /projects/b1108/projects/personalized_versus_group/scripts/process/sbatchinfo_9hr_10G_general.sh > '+get_surf_network_metrics_script)
        os.system('echo module purge >> '+get_surf_network_metrics_script)
        os.system('echo module load gdal/3.1.3-R-4.1.1 proj/7.1.1 geos/3.8.1 gsl/2.6-gcc-8.4.0 >> '+get_surf_network_metrics_script)
        os.system('echo module load R/4.3.0 >> '+get_surf_network_metrics_script)
        os.system('echo module load udunits2/2.2.20 >> '+get_surf_network_metrics_script)
        os.system('echo '+' '.join(cmd)+' >> '+get_surf_network_metrics_script)
        os.system('chmod +x '+get_surf_network_metrics_script)
        os.system('sbatch -o '+launchdir+sub+'_'+ses+'_get_surf_network_metrics_z1.txt'+' '+get_surf_network_metrics_script)
