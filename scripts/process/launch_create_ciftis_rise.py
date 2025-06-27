### This script creates output directories
###
### Ellyn Butler
### June 24, 2025 - June 26, 2025


import os
import shutil
import re
import numpy as np
import glob
import nibabel as nib

indir = '/projects/b1108/studies/rise/data/processed/neuroimaging/fmriprep_23.2.0/'
outdir = '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/surf/'
launchdir = '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/launch/surf/'

if not os.path.exists(outdir):
    os.mkdir(outdir)

if not os.path.exists(launchdir):
    os.mkdir(launchdir)

subdirs = glob.glob(indir + "sub-*[0-9]")

for subdir in subdirs:
    sub = subdir.split('/')[9]
    if not os.path.exists(outdir+sub):
        os.mkdir(outdir+sub)
    sessions = glob.glob(indir+sub+'/ses-*')
    sessions = [d for d in sessions if any(os.path.isdir(os.path.join(d, f)) for f in os.listdir(d))]
    for ses_path in sessions:
        ses = ses_path.split('/')[10]
        if not os.path.exists(outdir+sub+'/'+ses):
            os.mkdir(outdir+sub+'/'+ses)
    sub_bold_imgs = glob.glob(indir+sub+'/ses-*/func/*_space-T1w_desc-preproc_bold.nii.gz')
    if len(sub_bold_imgs) > 0:
        cmd = ['bash /projects/b1108/projects/personalized_versus_group/scripts/process/create_ciftis.sh -s', sub]
        create_ciftis_script = launchdir+sub+'_create_ciftis_run.sh'
        os.system('cat /projects/b1108/projects/personalized_versus_group/scripts/process/sbatchinfo_9hr_10G_general.sh > '+create_ciftis_script)
        os.system('echo '+' '.join(cmd)+' >> '+create_ciftis_script)
        os.system('chmod +x '+create_ciftis_script)
        os.system('sbatch -o '+launchdir+sub+'.txt'+' '+create_ciftis_script)
