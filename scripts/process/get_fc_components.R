### This script gets functional connectivity components from the 
### group functional connectivity estimates
###
### Ellyn Butler
### October 27, 2025 - December 14, 2025

set.seed(1432)

# Directories
indir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/'

# Load data
comp_df <- read.csv(paste0(indir, 'tabulated/components_2025-10-20.csv'))
clin_df <- read.csv(paste0(indir, 'tabulated/prior_subjects_2025-09-19.csv'))
net_df <- read.csv(paste0(indir, 'tabulated/surf_network_metrics_z1_2025-12-13.csv'))

df1 <- merge(comp_df, clin_df)
df1 <- merge(df1, net_df)

groupcols <- names(df1)[grepl('group', names(df1))]

netnames <- c('visuala', 'visualb', 'somatomotora', 'somatomotorb',
               'dorsalattentiona', 'dorsalattentionb', 'saliencea',
               'salienceb', 'limbica', 'limbicb', 'controla', 'controlb',
               'controlc', 'defaulta', 'defaultb', 'defaultc',
               'temporalparietal')

# Get xmat
xmat <- df1[, groupcols]

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

# V' (# of components by # of FCs) 
vtransmat <- diag(1/dvec) %*% t(umat) %*% xmat 
write.csv(vtransmat, paste0(indir, 'components/vtransmat_FC.csv'), row.names = FALSE)

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

write.csv(df, paste0(indir, 'tabulated/variance_explained_FC_svd.csv'), row.names = FALSE)

# Get scores for each subject
write.csv(umat, paste0(indir, 'tabulated/component_scores_FC_svd.csv'), row.names = FALSE)

