### This script creates ciftis from subject fmri data
###
### https://neurostars.org/t/volume-to-surface-mapping-mri-vol2surf-using-fmriprep-outputs/4079/13
### https://www.humanconnectome.org/software/workbench-command
### https://surfer.nmr.mgh.harvard.edu/fswiki/mri_vol2vol
### https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FNIRT/UserGuide#Now_what.3F_--_applywarp.21
### Resampling-FreeSurfer-HCP.pdf
###
### Ellyn Butler
### June 24, 2024 - July 1, 2025


while getopts ":s:" option; do
    case "${option}" in
        s) 
          sub="${OPTARG}"
          ;;
    esac
done

module load connectome_workbench/1.5.0

##### 0) set file paths
neurodir=/projects/b1108/studies/rise/data/processed/neuroimaging
outdir=/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging

# set output directories
anatoutdir=${outdir}/surf/${sub}/anat
mkdir ${anatoutdir}

# this is the feesurfer surf dir: for registration (spherical)
freedir=${neurodir}/fmriprep_23.2.0/sourcedata/freesurfer/${sub}

# hcp directory
hcptempdir=/projects/b1108/hcp/global/templates/standard_mesh_atlases/resample_fsaverage

##### 1) Convert freesurfer T1w image to a nifti
mri_convert ${freedir}/mri/T1.mgz ${outdir}/surf/${sub}/anat/fs_T1w.nii.gz

##### 2) convert .reg files into giftis
# left
wb_shortcuts -freesurfer-resample-prep ${freedir}/surf/lh.white ${freedir}/surf/lh.pial \
  ${freedir}/surf/lh.sphere.reg ${hcptempdir}/fs_LR-deformed_to-fsaverage.L.sphere.32k_fs_LR.surf.gii \
  ${anatoutdir}/${sub}.L.midthickness.native.surf.gii \
  ${anatoutdir}/${sub}.L.midthickness.32k_fs_LR.surf.gii \
  ${anatoutdir}/lh.sphere.reg.surf.gii

# right
wb_shortcuts -freesurfer-resample-prep ${freedir}/surf/rh.white ${freedir}/surf/rh.pial \
  ${freedir}/surf/rh.sphere.reg ${hcptempdir}/fs_LR-deformed_to-fsaverage.R.sphere.32k_fs_LR.surf.gii \
  ${anatoutdir}/${sub}.R.midthickness.native.surf.gii \
  ${anatoutdir}/${sub}.R.midthickness.32k_fs_LR.surf.gii \
  ${anatoutdir}/rh.sphere.reg.surf.gii

# Get the correct number of sessions (surprisingly complicated)
sessions_func=`find ${neurodir}/fmriprep_23.2.0/${sub}/ses-* -name "func" | cut -d "/" -f 11`
sessions_anat=`find ${neurodir}/fmriprep_23.2.0/${sub}/ses-* -name "anat" | cut -d "/" -f 11`
numses_func=`echo ${sessions_func} | grep -o 'ses-[^ ]*' | wc -l`
numses_anat=`echo ${sessions_anat} | grep -o 'ses-[^ ]*' | wc -l`

if [ "${numses_func}" -gt "${numses_anat}" ]; then
  numses=${numses_func}
  sessions=${sessions_func}
else
  numses=${numses_anat}
  sessions=${sessions_anat}
fi

for ses in ${sessions}; do
    echo ${ses}
    task_paths=`find ${neurodir}/fmriprep_23.2.0/${sub}/${ses}/func/ -name "*_space-T1w_desc-preproc_bold.nii.gz"`
    taskruns=""
    for task_path in ${task_paths}; do
      thistask=`echo ${task_path} | cut -d "/" -f 13 | cut -d "_" -f 3`
      thisrun=`echo ${task_path} | cut -d "/" -f 13 | cut -d "_" -f 4`
      this=${thistask}_${thisrun}
      taskruns="${this} ${taskruns}"
    done

    # set input directories
    if [ ${numses} == 1 ]; then
      anatindir=${neurodir}/fmriprep_23.2.0/${sub}/${ses}/anat
    else
      anatindir=${neurodir}/fmriprep_23.2.0/${sub}/anat
    fi
    funcindir=${neurodir}/fmriprep_23.2.0/${sub}/${ses}/func

    # set output directories
    funcoutdir=${outdir}/surf/${sub}/${ses}/func
    mkdir ${funcoutdir}

    for taskrun in ${taskruns}; do
      echo ${taskrun}
      # set t1 space fmri volume location
      VolumefMRI=${funcindir}/${sub}_${ses}_${taskrun}_space-T1w_desc-preproc_bold.nii.gz

      # this one is to-be-created as an intermediate
      nativesurfMRI_L=${funcoutdir}/${sub}_${ses}_${taskrun}.L.native.func.gii
      nativesurfMRI_R=${funcoutdir}/${sub}_${ses}_${taskrun}.R.native.func.gii

      # and then these are output
      fslrfMRI_L=${funcoutdir}/${sub}_${ses}_${taskrun}.L.fslr.func.gii
      fslrfMRI_R=${funcoutdir}/${sub}_${ses}_${taskrun}.R.fslr.func.gii

      ##### 3) map t1-space bold to native freesurfer (note: no -volume-roi flag, assuming this is an SNR mask)
      # left
      sub_midthick_L=`find ${anatindir}/ -name "*_hemi-L_midthickness.surf.gii"`
      sub_white_L=`find ${anatindir}/ -name "*_hemi-L_white.surf.gii"`
      sub_pial_L=`find ${anatindir}/ -name "*_hemi-L_pial.surf.gii"`
      wb_command -volume-to-surface-mapping ${VolumefMRI} ${sub_midthick_L} \
        ${nativesurfMRI_L} -ribbon-constrained ${sub_white_L} ${sub_pial_L}

      # right
      sub_midthick_R=`find ${anatindir}/ -name "*_hemi-R_midthickness.surf.gii"`
      sub_white_R=`find ${anatindir}/ -name "*_hemi-R_white.surf.gii"`
      sub_pial_R=`find ${anatindir}/ -name "*_hemi-R_pial.surf.gii"`
      wb_command -volume-to-surface-mapping ${VolumefMRI} ${sub_midthick_R} \
        ${nativesurfMRI_R} -ribbon-constrained ${sub_white_R} ${sub_pial_R}

      ##### 4) dilate by ten, consistent b/w fmriprep and dcan hcp pipeline
      # (would love to know how they converged on this value. Note: input and output are same)
      # left
      wb_command -metric-dilate ${nativesurfMRI_L} \
        ${sub_midthick_L} 10 \
        ${nativesurfMRI_L} -nearest

      # right
      wb_command -metric-dilate ${nativesurfMRI_R} \
        ${sub_midthick_R} 10 \
        ${nativesurfMRI_R} -nearest

      ##### 5) resample native surface to fslr
      # (note: omission of roi use again)
      # left
      wb_command -metric-resample ${nativesurfMRI_L} ${anatoutdir}/lh.sphere.reg.surf.gii \
        ${hcptempdir}/fs_LR-deformed_to-fsaverage.L.sphere.32k_fs_LR.surf.gii BARYCENTRIC \
        ${fslrfMRI_L} 

      # right
      wb_command -metric-resample ${nativesurfMRI_R} ${anatoutdir}/rh.sphere.reg.surf.gii \
        ${hcptempdir}/fs_LR-deformed_to-fsaverage.R.sphere.32k_fs_LR.surf.gii BARYCENTRIC \
        ${fslrfMRI_R} 

      ##### 6) Set the structure parameter so that wb_view knows how to display the data
      wb_command -set-structure ${fslrfMRI_L} CORTEX_LEFT
      wb_command -set-structure ${fslrfMRI_R} CORTEX_RIGHT

      ##### 7) Convert from gifti to cifti
      # https://neurostars.org/t/any-way-to-convert-a-metric-gifti-to-a-scalar-cifti/19623
      wb_command -cifti-create-dense-scalar ${funcoutdir}/${sub}_${ses}_${taskrun}_space-fsLR_desc-preproc_bold.dscalar.nii \
        -left-metric ${fslrfMRI_L} \
        -right-metric ${fslrfMRI_R}
  done
done