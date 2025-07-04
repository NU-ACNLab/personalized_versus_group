### This script generates submission scripts for fmriprep
###
### Ellyn Butler
### June 27, 2025

# --output-spaces fsLR --cifti-output 91k (--project-goodvoxels)

import os
import shutil
import re
import numpy as np
import glob

indir = '/projects/b1108/studies/crest/data/raw/neuroimaging/bids/'
outdir = '/projects/b1108/studies/crest/data/processed/neuroimaging/fmriprep_23.2.0/'
launchdir = '/projects/b1108/studies/crest/data/processed/neuroimaging/launch/fmriprep_23.2.0/'
workdir = '/projects/b1108/studies/crest/data/processed/neuroimaging/work2/'

subdirs = glob.glob(indir + "sub-*")

for subdir in subdirs:
    sub = subdir.split('/')[9]
    if not os.path.exists(outdir+sub):
        os.mkdir(outdir+sub)
    participant_label = sub.split('-')[1]
    # Check if the subject has already finished processing (couldn't have if they don't have an html)
    if not os.path.exists(outdir+sub+'.html'):
        sessions = os.listdir(indir+sub)
        txtlog = launchdir+sub+'.txt'
        # If there are any errors in the log file, delete the working directory
        # and the output directory
        if os.path.exists(txtlog):
            with open(txtlog) as myfile:
                if 'ERROR' in myfile.read():
                    subworkdir = workdir+'fmriprep_wf/single_subject_'+participant_label+'_wf'
                    if os.path.exists(subworkdir):
                        shutil.rmtree(subworkdir)
                        shutil.rmtree(outdir+sub)
                    print(sub)
        # run!
        if len(sessions) > 1:
            for ses in sessions:
                if not os.path.exists(outdir+sub+'/'+ses):
                    os.mkdir(outdir+sub+'/'+ses)
            cmd = ['SINGULARITYENV_TEMPLATEFLOW_HOME=/home/fmriprep/.cache/templateflow',
                'singularity', 'run', '--writable-tmpfs', '--cleanenv', '--containall',
                '-B /tmp:/tmp', '-B /projects/b1108:/projects/b1108',
                '-B /projects/b1108/software/freesurfer_license/license.txt:/opt/freesurfer/license.txt',
                '-B /projects/b1108/templateflow:/home/fmriprep/.cache/templateflow',
                '/projects/b1108/software/singularity_images/fmriprep_23.2.0.sif',
                indir, outdir, 'participant', '--participant-label',
                participant_label, '--longitudinal', '--nprocs=1 --omp-nthreads=1',
                '-w ', workdir, '--skip_bids_validation',
                '--fs-license-file /opt/freesurfer/license.txt',
                '--use-syn-sdc', '--force-syn',
                '--output-spaces MNI152NLin6Asym:res-2 anat',
                '--skull-strip-template OASIS30ANTs']
            fmriprep_script = launchdir+sub+'_fmriprep_run.sh'
            os.system('cat /projects/b1108/studies/crest/scripts/process/sbatchinfo_general_extralong.sh > '+fmriprep_script) #_long
            os.system('echo '+' '.join(cmd)+' >> '+fmriprep_script)
            os.system('chmod +x '+fmriprep_script)
            os.system('sbatch -o '+launchdir+sub+'.txt'+' '+fmriprep_script)
        else:
            ses = sessions[0]
            if not os.path.exists(outdir+sub+'/'+ses):
                os.mkdir(outdir+sub+'/'+ses)
            participant_label = sub.split('-')[1]
            cmd = ['SINGULARITYENV_TEMPLATEFLOW_HOME=/home/fmriprep/.cache/templateflow',
                'singularity', 'run', '--writable-tmpfs', '--cleanenv', '--containall',
                '-B /tmp:/tmp', '-B /projects/b1108:/projects/b1108',
                '-B /projects/b1108/software/freesurfer_license/license.txt:/opt/freesurfer/license.txt',
                '-B /projects/b1108/templateflow:/home/fmriprep/.cache/templateflow',
                '/projects/b1108/software/singularity_images/fmriprep_23.2.0.sif',
                indir, outdir, 'participant', '--participant-label', participant_label,
                '--nprocs=1 --omp-nthreads=1',
                '-w ', workdir, '--skip_bids_validation',
                '--fs-license-file /opt/freesurfer/license.txt',
                '--use-syn-sdc', '--force-syn',
                '--output-spaces MNI152NLin6Asym:res-2 anat',
                '--skull-strip-template OASIS30ANTs']
            fmriprep_script = launchdir+sub+'_fmriprep_run.sh'
            os.system('cat /projects/b1108/studies/crest/scripts/process/sbatchinfo_general_extralong.sh > '+fmriprep_script)
            os.system('echo '+' '.join(cmd)+' >> '+fmriprep_script)
            os.system('chmod +x '+fmriprep_script)
            os.system('sbatch -o '+launchdir+sub+'.txt'+' '+fmriprep_script)
    # if the subject does have an html...
    else:
        # delete working directory
        participant_label = sub.split('-')[1]
        #subworkdir = workdir+'fmriprep_wf/single_subject_'+participant_label+'_wf'
        #if os.path.exists(subworkdir):
        #    shutil.rmtree(subworkdir)
        # but if there are errors in the log file, rerun with more memory
        txtlog = launchdir+sub+'.txt'
        with open(txtlog) as myfile:
            if 'ERROR' in myfile.read():
                # delete faulty output
                #shutil.rmtree(outdir+sub)
                #os.mkdir(outdir+sub)
                #print(sub)
                # delete old log
                #os.remove(txtlog)
                sessions = os.listdir(indir+sub)
                if len(sessions) > 1:
                    for ses in sessions:
                        if not os.path.exists(outdir+sub+'/'+ses):
                            os.mkdir(outdir+sub+'/'+ses)
                    cmd = ['SINGULARITYENV_TEMPLATEFLOW_HOME=/home/fmriprep/.cache/templateflow',
                        'singularity', 'run', '--writable-tmpfs', '--cleanenv', '--containall',
                        '-B /tmp:/tmp', '-B /projects/b1108:/projects/b1108',
                        '-B /projects/b1108/software/freesurfer_license/license.txt:/opt/freesurfer/license.txt',
                        '-B /projects/b1108/templateflow:/home/fmriprep/.cache/templateflow',
                        '/projects/b1108/software/singularity_images/fmriprep_23.2.0.sif',
                        indir, outdir, 'participant', '--participant-label',
                        participant_label, '--longitudinal', '--nprocs=1 --omp-nthreads=1',
                        '-w ', workdir, '--skip_bids_validation',
                        '--fs-license-file /opt/freesurfer/license.txt',
                        '--use-syn-sdc', '--force-syn',
                        '--output-spaces MNI152NLin6Asym:res-2 anat',
                        '--skull-strip-template OASIS30ANTs']
                    fmriprep_script = launchdir+sub+'_fmriprep_run.sh'
                    os.system('cat /projects/b1108/studies/crest/scripts/process/sbatchinfo_general_extralong30.sh > '+fmriprep_script) #_long
                    os.system('echo '+' '.join(cmd)+' >> '+fmriprep_script)
                    os.system('chmod +x '+fmriprep_script)
                    os.system('sbatch -o '+launchdir+sub+'.txt'+' '+fmriprep_script)
                else:
                    ses = sessions[0]
                    if not os.path.exists(outdir+sub+'/'+ses):
                        os.mkdir(outdir+sub+'/'+ses)
                    participant_label = sub.split('-')[1]
                    cmd = ['SINGULARITYENV_TEMPLATEFLOW_HOME=/home/fmriprep/.cache/templateflow',
                        'singularity', 'run', '--writable-tmpfs', '--cleanenv', '--containall',
                        '-B /tmp:/tmp', '-B /projects/b1108:/projects/b1108',
                        '-B /projects/b1108/software/freesurfer_license/license.txt:/opt/freesurfer/license.txt',
                        '-B /projects/b1108/templateflow:/home/fmriprep/.cache/templateflow',
                        '/projects/b1108/software/singularity_images/fmriprep_23.2.0.sif',
                        indir, outdir, 'participant', '--participant-label', participant_label,
                        '--nprocs=1 --omp-nthreads=1',
                        '-w ', workdir, '--skip_bids_validation',
                        '--fs-license-file /opt/freesurfer/license.txt',
                        '--use-syn-sdc', '--force-syn',
                        '--output-spaces MNI152NLin6Asym:res-2 anat',
                        '--skull-strip-template OASIS30ANTs']
                    fmriprep_script = launchdir+sub+'_fmriprep_run.sh'
                    os.system('cat /projects/b1108/studies/crest/scripts/process/sbatchinfo_general_extralong30.sh > '+fmriprep_script)
                    os.system('echo '+' '.join(cmd)+' >> '+fmriprep_script)
                    os.system('chmod +x '+fmriprep_script)
                    os.system('sbatch -o '+launchdir+sub+'.txt'+' '+fmriprep_script)

i=0
for subdir in subdirs:
    sub = subdir.split('/')[9]
    txtlog = launchdir+sub+'.txt'
    with open(txtlog) as myfile:
        if 'ERROR' in myfile.read():
            print(sub)
            i=i+1 #June 24, 2025: No errors!

for subdir in subdirs:
    sub = subdir.split('/')[9]
    if not os.path.exists(outdir+sub):
        os.mkdir(outdir+sub)
    participant_label = sub.split('-')[1]
    # Check if the subject has already finished processing (couldn't have if they don't have an html)
    if not os.path.exists(outdir+sub+'.html'):
        print(sub)