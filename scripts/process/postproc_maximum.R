### This script does maximum postprocessing on the task data
###
### Ellyn Butler
### July 4, 2025

##### Load packages
library(argparse)
library(stats)
library(fMRIscrub)
library(fMRItools)
library(ciftiTools)
library(stringr)
ciftiTools.setOption('wb_path', '/projects/b1108/software/workbench')

##### Parse command line options
parser <- ArgumentParser()
parser$add_argument('-s', '--subid', type='character', help='Subject Identifier')
parser$add_argument('-e', '--sesid', type='character', help='Session Identifier')

args <- parser$parse_args()

subid = args$subid 
sesid = args$sesid

##### Set directories
indir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/'
#indir <- '/Users/flutist4129/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/'

rise_motdir <- '/projects/b1108/studies/rise/data/processed/neuroimaging/fmriprep_23.2.0/'
#rise_motdir <- '/Users/flutist4129/Documents/Northwestern/studies/rise/data/processed/neuroimaging/fmriprep_23.2.0/'
crest_motdir <- '/projects/b1108/studies/crest/data/processed/neuroimaging/fmriprep_23.2.0/'
#crest_motdir <- '/Users/flutist4129/Documents/Northwestern/studies/crest/data/processed/neuroimaging/fmriprep_23.2.0/'

rise_dirs <- list.dirs(rise_motdir, recursive = FALSE)
if (paste0(rise_motdir, '/sub-', subid) %in% rise_dirs) {
    motdir <- rise_motdir
} else {
    motdir <- crest_motdir
}

##### Find tasks
dscalars <- system(paste0('find ', indir,'surf/sub-', subid, '/ses-', sesid, 
            '/func -name "*_space-fsLR_desc-preproc_bold.dscalar.nii"'), intern = TRUE)

##### GPARC
GPARC <- readRDS(paste0(indir, 'template/GPARC.rds'))
mwall_L <- GPARC$meta$cortex$medial_wall$left
mwall_R <- GPARC$meta$cortex$medial_wall$right

##### Quality
qual_df <- data.frame(subid = character(), 
                      sesid = character(),
                      taskrun = character(),
                      nTRs = integer(),
                      nRegressors = integer(),
                      stringsAsFactors = FALSE
                     )

##### Loop over tasks
for (dscalar in dscalars) {
    ##### Post-process
    cii <- read_cifti(dscalar)
    taskrun <- str_extract(dscalar, "task-[^_]+_run-[^_]+")

    # Mask out medial wall
    cii$data$cortex_left[!mwall_L,] <- NA
    cii$data$cortex_right[!mwall_R,] <- NA
    cii <- move_to_mwall(cii, values = NA)

    # Get dimensions
    x <- t(rbind(cii$data$cortex_left, cii$data$cortex_right))
    nT <- nrow(x)
    nV <- ncol(x)

    # Flagging
    dv <- DVARS(x)
    dv_flag <- dv$outlier_flag$Dual
    dv_nS <- sum(dv_flag)

    # One-hot encode outlier flags
    dv_spikes <- matrix(0, nrow=nT, ncol=dv_nS)
    dv_spikes[seq(0, dv_nS-1)*nT + which(dv_flag)] <- 1

    # Select motion regressors
    rp <- read.delim(paste0(motdir, 'sub-', subid, '/ses-', sesid, '/func/sub-', subid, 
                    '_ses-', sesid, '_', taskrun, '_desc-confounds_timeseries.tsv'), sep = '\t')
    rp <- rp[, c(paste0('trans_', c('x', 'y', 'z')), paste0('rot_', c('x', 'y', 'z')),
                paste0('trans_', c('x', 'y', 'z'), '_derivative1'), paste0('rot_', c('x', 'y', 'z'), '_derivative1'), 
                paste0('trans_', c('x', 'y', 'z'), '_power2'), paste0('rot_', c('x', 'y', 'z'), '_power2'),
                paste0('trans_', c('x', 'y', 'z'), '_derivative1_power2'), paste0('rot_', c('x', 'y', 'z'), '_derivative1_power2'),
                paste0('global_signal', c('_derivative1', '_power2', '_derivative1_power2')))]

    # Set filtering parameters
    dct <- dct_bases(nT, dct_convert(nT, TR=2.05, f=.01)) # .01 Hz HPF, 11

    # Nuisance regression
    nreg <- cbind(dv_spikes, rp, dct)
    nreg[nreg == 'n/a'] <- 0
    x_reg <- nuisance_regression(x, nreg)[!dv_flag,,drop=FALSE]
    cii_out <- cii

    nVertices_left <- nrow(cii$data$cortex_left)
    cii_out$data$cortex_left <- t(x_reg[, 1:nVertices_left])
    cii_out$data$cortex_right <- t(x_reg[, (nVertices_left+1):ncol(x_reg)])

    # Downsample
    cii_out$meta$cifti$names <- cii_out$meta$cifti$names[1:sum(!dv_flag)]
    cii_out <- resample_cifti(cii_out, resamp_res = 10000)

    # Write out image
    write_cifti(cii_out, paste0(indir, 'surf/sub-', subid, '/ses-', sesid, '/func/sub-', subid, 
                    '_ses-', sesid, '_', taskrun, '_space-fsLR_desc-maxpostproc_bold.dscalar.nii'))

    # Add columns of number of regressors, and original number of TRs
    qual_df <- rbind(qual_df, data.frame(subid = subid, sesid = sesid, taskrun = taskrun, 
                                nTRs = nT, nRegressors = ncol(nreg)))
}

# Write out csv of number of regressors, and original number of TRs
write.csv(qual_df, paste0(indir, 'surf/sub-', subid, '/ses-', sesid, '/func/sub-', subid, 
                    '_ses-', sesid, '_quality.csv'), row.names = FALSE)