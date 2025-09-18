### This script generates submission scripts for postprocessing
###
### Ellyn Butler
### July 4, 2025 - September 18, 2025


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
    #if (taskchatroom_run01_ses1_infile and not taskchatroom_run01_ses1_outfile) or (taskmid_run01_ses1_infile and not taskmid_run01_ses1_outfile) or (taskmid_run02_ses1_infile and not taskmid_run02_ses1_outfile): 
    if taskchatroom_run01_ses1_infile or taskmid_run01_ses1_infile or taskmid_run02_ses1_infile: 
        ses = 'ses-1'
        cmd = ['Rscript /projects/b1108/projects/personalized_versus_group/scripts/process/postproc_maximum.R -s ', subid, ' -e 1']
        postproc_maximum_script = launchdir+sub+'_'+ses+'_postproc_maximum_run.sh'
        os.system('cat /projects/b1108/projects/personalized_versus_group/scripts/process/sbatchinfo_40min_10G_general.sh > '+postproc_maximum_script)
        os.system('echo '+' '.join(cmd)+' >> '+postproc_maximum_script)
        os.system('chmod +x '+postproc_maximum_script)
        os.system('sbatch -o '+launchdir+sub+'_'+ses+'_postproc_maximum.txt'+' '+postproc_maximum_script)
    # ses-2
    taskchatroom_run01_ses2_infile = os.path.exists(indir + sub + '/ses-2/func/' + sub + '_ses-2_task-chatroom_run-01_space-fsLR_desc-preproc_bold.dscalar.nii')
    taskchatroom_run01_ses2_outfile = os.path.exists(indir + sub + '/ses-2/func/' + sub + '_ses-2_task-chatroom_run-01_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    taskmid_run01_ses2_infile = os.path.exists(indir + sub + '/ses-2/func/' + sub + '_ses-2_task-mid_run-01_space-fsLR_desc-preproc_bold.dscalar.nii')
    taskmid_run01_ses2_outfile = os.path.exists(indir + sub + '/ses-2/func/' + sub + '_ses-2_task-mid_run-01_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    taskmid_run02_ses2_infile = os.path.exists(indir + sub + '/ses-2/func/' + sub + '_ses-2_task-mid_run-02_space-fsLR_desc-preproc_bold.dscalar.nii')
    taskmid_run02_ses2_outfile = os.path.exists(indir + sub + '/ses-2/func/' + sub + '_ses-2_task-mid_run-02_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    #if (taskchatroom_run01_ses2_infile and not taskchatroom_run01_ses2_outfile) or (taskmid_run01_ses2_infile and not taskmid_run01_ses2_outfile) or (taskmid_run02_ses2_infile and not taskmid_run02_ses2_outfile):
    if taskchatroom_run01_ses2_infile or taskmid_run01_ses2_infile or taskmid_run02_ses2_infile:  
        ses = 'ses-2'
        cmd = ['Rscript /projects/b1108/projects/personalized_versus_group/scripts/process/postproc_maximum.R -s ', subid, ' -e 2']
        postproc_maximum_script = launchdir+sub+'_'+ses+'_postproc_maximum_run.sh'
        os.system('cat /projects/b1108/projects/personalized_versus_group/scripts/process/sbatchinfo_40min_10G_general.sh > '+postproc_maximum_script)
        os.system('echo '+' '.join(cmd)+' >> '+postproc_maximum_script)
        os.system('chmod +x '+postproc_maximum_script)
        os.system('sbatch -o '+launchdir+sub+'_'+ses+'_postproc_maximum.txt'+' '+postproc_maximum_script)
    # ses-3
    taskchatroom_run01_ses3_infile = os.path.exists(indir + sub + '/ses-3/func/' + sub + '_ses-3_task-chatroom_run-01_space-fsLR_desc-preproc_bold.dscalar.nii')
    taskchatroom_run01_ses3_outfile = os.path.exists(indir + sub + '/ses-3/func/' + sub + '_ses-3_task-chatroom_run-01_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    taskmid_run01_ses3_infile = os.path.exists(indir + sub + '/ses-3/func/' + sub + '_ses-3_task-mid_run-01_space-fsLR_desc-preproc_bold.dscalar.nii')
    taskmid_run01_ses3_outfile = os.path.exists(indir + sub + '/ses-3/func/' + sub + '_ses-3_task-mid_run-01_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    taskmid_run02_ses3_infile = os.path.exists(indir + sub + '/ses-3/func/' + sub + '_ses-3_task-mid_run-02_space-fsLR_desc-preproc_bold.dscalar.nii')
    taskmid_run02_ses3_outfile = os.path.exists(indir + sub + '/ses-3/func/' + sub + '_ses-3_task-mid_run-02_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
    #if (taskchatroom_run01_ses3_infile and not taskchatroom_run01_ses3_outfile) or (taskmid_run01_ses3_infile and not taskmid_run01_ses3_outfile) or (taskmid_run02_ses3_infile and not taskmid_run02_ses3_outfile):
    if taskchatroom_run01_ses3_infile or taskmid_run01_ses3_infile or taskmid_run02_ses3_infile: 
        ses = 'ses-3'
        cmd = ['Rscript /projects/b1108/projects/personalized_versus_group/scripts/process/postproc_maximum.R -s ', subid, ' -e 3']
        postproc_maximum_script = launchdir+sub+'_'+ses+'_postproc_maximum_run.sh'
        os.system('cat /projects/b1108/projects/personalized_versus_group/scripts/process/sbatchinfo_40min_10G_general.sh > '+postproc_maximum_script)
        os.system('echo '+' '.join(cmd)+' >> '+postproc_maximum_script)
        os.system('chmod +x '+postproc_maximum_script)
        os.system('sbatch -o '+launchdir+sub+'_'+ses+'_postproc_maximum.txt'+' '+postproc_maximum_script)

i=0
for subdir in subdirs:
    sub = subdir.split('/')[9]
    txtlog = launchdir+sub+'.txt'
    if os.path.exists(txtlog):
        with open(txtlog) as myfile:
            if 'ERROR' in myfile.read():
                print(subdir)
                i=i+1 
    else:
        print(sub + ' processing does not exist')

# July 6, 2025 - sanity check
# sub-10.21 processing does not exist - good, trial sub
# sub-50133 processing does not exist - good, only has a T1w image
# sub-1.9.2 processing does not exist - good, trial sub
# sub-100333 processing does not exist - good, scan was lost due to scanner upgrade
# sub-100337 processing does not exist - good, failed freesurfer at fmriprep stage
# sub-50001 processing does not exist - investigate... ciftis hadn't been created... exists now