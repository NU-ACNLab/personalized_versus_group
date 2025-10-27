### This script gets spatial components from the 
### engagement maps via singular value decomposition
###
### Ellyn Butler
### October 15, 2025 - October 27, 2025

set.seed(1432)

# Load libraries
library(ciftiTools)
ciftiTools.setOption('wb_path', '/projects/b1108/software/workbench')

# Directories
indir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/'

# Load prior
prior <- readRDS(paste0(indir, 'prior/prior_task-all.rds'))

# Load data
temp_subjs <- read.csv(paste0(indir, 'tabulated/prior_subjects_2025-09-19.csv'))
sesid <- 1
for (i in 1:nrow(temp_subjs)) { 
    subid <- temp_subjs[i, 'subid']
    path <- c(system(paste0('find ', indir, 'surfnet/sub-', subid, '/ses-', sesid,
            ' -name "networks_img.rds"'), intern=TRUE))
    img <- readRDS(path)
    for (j in 1:17) {
        vec <- c(img$subjNet_mean$data$cortex_left[, j], img$subjNet_mean$data$cortex_right[, j])
        assign(paste0('vec_i', i, '_j', j), vec)
    }
}

netnames <- c('VisCent', 'VisCeri', 'SomMotA', 'SomMotB', 'DorsAttnA',
              'DorsAttnB', 'SalVenAttnA', 'SalVenAttnB', 'LimbicA',
              'LimbicB', 'ContA', 'ContB', 'ContC', 'DefaultA',
              'DefaultB', 'DefaultC', 'TempPar')
for (j in 1:17) {
  # Get the prior mean and sd for this network
  # (mean - mean of the engagement values for a particular network)
  # (sd - sd of the engagement values for a particular network)
  mthresh <- mean(prior$prior$mean[,j])
  sdthresh <- sd(prior$prior$mean[,j])
  thresh <- mthresh + 2*sdthresh

  # Create the matrix of subjects' vertices for this network
  vec_list <- mget(paste0('vec_i', 1:nrow(temp_subjs), '_j', j))
  xmat <- do.call(rbind, vec_list)

  # Threshold
  xmat[xmat < thresh] <- 0

  # Center across subjects (rows are subjects)
  xmat <- scale(xmat, center = TRUE, scale = FALSE)

  # XX' (# of subjects by # of subjects) 
  xxtransmat <- tcrossprod(xmat)

  # U (# of subjects by # of components)
  #Note: U is orthogonal
  #Note: # of components = # of subjects here
  svd_out <- svd(xxtransmat)
  umat <- svd_out$u #correct dimensions

  # D (# of components by # of components)... just changes scale of columns if post-multiply with U
  dvec <- sqrt(svd_out$d)
  dmat <- diag(dvec) 

  # V' (# of components by # of vertices) 
  vtransmat <- diag(1/dvec) %*% t(umat) %*% xmat 
  write.csv(vtransmat[1:20, ], paste0(indir, 'components/', netnames[j], '_thresholded.csv'), row.names = FALSE)

  # Total variance
  totvar <- sum(dmat[!is.na(dmat)]^2)

  # Variance explained by each components (same as singular values, svd_out$d)
  ddiag <- diag(dmat)
  dsq <- ddiag^2
  varex <- dsq/totvar
  varex <- varex[!is.na(varex)]

  df <- data.frame(component = 1:length(varex),
                  variance_explained = varex,
                  cumulative_variance_explained = cumsum(varex))

  write.csv(df, paste0(indir, 'tabulated/variance_explained_thresholded_j', j, '_svd.csv'), row.names = FALSE)

  # Get scores for each subject
  write.csv(umat, paste0(indir, 'tabulated/component_scores_thresholded_', netnames[j], '_svd.csv'), row.names = FALSE)
}

# Save workspace for future tinkering (delete/modify later)
#save.image('/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/tmp/svd_by_network_thresholded.RData')

