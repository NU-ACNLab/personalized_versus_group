### This script generates submission scripts for postprocessing
###
### Ellyn Butler
### July 4, 2025


import os
import shutil
import re
import numpy as np
import glob
import nibabel as nib
import pandas as pd

indir = '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/surf/'
outdir = '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/surf/'
launchdir = '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/launch/surf/'

if not os.path.exists(outdir):
    os.mkdir(outdir)

if not os.path.exists(launchdir):
    os.mkdir(launchdir)

subdirs = glob.glob(indir + "sub-*")


for subdir in subdirs:
    sub = subdir.split('/')[9]
    subid = sub.split('-')[1]
    # ses-1
    taskchatroom_run01_ses1_infile = os.path.exists(indir + sub + '/ses-1/func/' + sub + '_ses-1_task-chatroom_run-01_space-fsLR_desc-preproc_bold.dscalar.nii')
    taskchatroom_run01_ses1_outfile = os.path.exists(indir + sub + '/ses-1/func/' + sub + '_ses-1_task-chatroom_run-01_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    taskmid_run01_ses1_infile = os.path.exists(indir + sub + '/ses-1/func/' + sub + '_ses-1_task-mid_run-01_space-fsLR_desc-preproc_bold.dscalar.nii')
    taskmid_run01_ses1_outfile = os.path.exists(indir + sub + '/ses-1/func/' + sub + '_ses-1_task-mid_run-01_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    taskmid_run02_ses1_infile = os.path.exists(indir + sub + '/ses-1/func/' + sub + '_ses-1_task-mid_run-02_space-fsLR_desc-preproc_bold.dscalar.nii')
    taskmid_run02_ses1_outfile = os.path.exists(indir + sub + '/ses-1/func/' + sub + '_ses-1_task-mid_run-02_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    if (taskchatroom_run01_ses1_infile and not taskchatroom_run01_ses1_outfile) or (taskmid_run01_ses1_infile and not taskmid_run01_ses1_outfile) or (taskmid_run02_ses1_infile and not taskmid_run02_ses1_outfile): 
        ses = 'ses-1'
        cmd = ['Rscript /projects/b1108/projects/personalized_versus_group/scripts/process/postproc_rest_maximum.R -s ', subid, ' -e 1']
        postproc_rest_maximum_script = launchdir+sub+'_'+ses+'_postproc_rest_maximum_run.sh'
        os.system('cat /projects/b1108/projects/personalized_versus_group/scripts/process/sbatchinfo_20min_10G_general.sh > '+postproc_rest_maximum_script)
        os.system('echo '+' '.join(cmd)+' >> '+postproc_rest_maximum_script)
        os.system('chmod +x '+postproc_rest_maximum_script)
        os.system('sbatch -o '+launchdir+sub+'_'+ses+'_postproc_rest_maximum.txt'+' '+postproc_rest_maximum_script)
    # ses-2
    taskchatroom_run01_ses2_infile = os.path.exists(indir + sub + '/ses-2/func/' + sub + '_ses-2_task-chatroom_run-01_space-fsLR_desc-preproc_bold.dscalar.nii')
    taskchatroom_run01_ses2_outfile = os.path.exists(indir + sub + '/ses-2/func/' + sub + '_ses-2_task-chatroom_run-01_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    taskmid_run01_ses2_infile = os.path.exists(indir + sub + '/ses-2/func/' + sub + '_ses-2_task-mid_run-01_space-fsLR_desc-preproc_bold.dscalar.nii')
    taskmid_run01_ses2_outfile = os.path.exists(indir + sub + '/ses-2/func/' + sub + '_ses-2_task-mid_run-01_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    taskmid_run02_ses2_infile = os.path.exists(indir + sub + '/ses-2/func/' + sub + '_ses-2_task-mid_run-02_space-fsLR_desc-preproc_bold.dscalar.nii')
    taskmid_run02_ses2_outfile = os.path.exists(indir + sub + '/ses-2/func/' + sub + '_ses-2_task-mid_run-02_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    if (taskchatroom_run01_ses2_infile and not taskchatroom_run01_ses2_outfile) or (taskmid_run01_ses2_infile and not taskmid_run01_ses2_outfile) or (taskmid_run02_ses2_infile and not taskmid_run02_ses2_outfile): 
        ses = 'ses-2'
        cmd = ['Rscript /projects/b1108/projects/personalized_versus_group/scripts/process/postproc_rest_maximum.R -s ', subid, ' -e 2']
        postproc_rest_maximum_script = launchdir+sub+'_'+ses+'_postproc_rest_maximum_run.sh'
        os.system('cat /projects/b1108/projects/personalized_versus_group/scripts/process/sbatchinfo_20min_10G_general.sh > '+postproc_rest_maximum_script)
        os.system('echo '+' '.join(cmd)+' >> '+postproc_rest_maximum_script)
        os.system('chmod +x '+postproc_rest_maximum_script)
        os.system('sbatch -o '+launchdir+sub+'_'+ses+'_postproc_rest_maximum.txt'+' '+postproc_rest_maximum_script)
    # ses-3
    taskchatroom_run01_ses3_infile = os.path.exists(indir + sub + '/ses-3/func/' + sub + '_ses-3_task-chatroom_run-01_space-fsLR_desc-preproc_bold.dscalar.nii')
    taskchatroom_run01_ses3_outfile = os.path.exists(indir + sub + '/ses-3/func/' + sub + '_ses-3_task-chatroom_run-01_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    taskmid_run01_ses3_infile = os.path.exists(indir + sub + '/ses-3/func/' + sub + '_ses-3_task-mid_run-01_space-fsLR_desc-preproc_bold.dscalar.nii')
    taskmid_run01_ses3_outfile = os.path.exists(indir + sub + '/ses-3/func/' + sub + '_ses-3_task-mid_run-01_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    taskmid_run02_ses3_infile = os.path.exists(indir + sub + '/ses-3/func/' + sub + '_ses-3_task-mid_run-02_space-fsLR_desc-preproc_bold.dscalar.nii')
    taskmid_run02_ses3_outfile = os.path.exists(indir + sub + '/ses-3/func/' + sub + '_ses-3_task-mid_run-02_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    if (taskchatroom_run01_ses3_infile and not taskchatroom_run01_ses3_outfile) or (taskmid_run01_ses3_infile and not taskmid_run01_ses3_outfile) or (taskmid_run02_ses3_infile and not taskmid_run02_ses3_outfile): 
        ses = 'ses-3'
        cmd = ['Rscript /projects/b1108/projects/personalized_versus_group/scripts/process/postproc_rest_maximum.R -s ', subid, ' -e 3']
        postproc_rest_maximum_script = launchdir+sub+'_'+ses+'_postproc_rest_maximum_run.sh'
        os.system('cat /projects/b1108/projects/personalized_versus_group/scripts/process/sbatchinfo_20min_10G_general.sh > '+postproc_rest_maximum_script)
        os.system('echo '+' '.join(cmd)+' >> '+postproc_rest_maximum_script)
        os.system('chmod +x '+postproc_rest_maximum_script)
        os.system('sbatch -o '+launchdir+sub+'_'+ses+'_postproc_rest_maximum.txt'+' '+postproc_rest_maximum_script)
