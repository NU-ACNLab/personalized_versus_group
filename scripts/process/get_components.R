### This script gets spatial components from the 
### engagement maps via singular value decomposition
###
### Ellyn Butler
### October 2, 2025

# Load libraries
library(ciftiTools)
ciftiTools.setOption('wb_path', '/projects/b1108/software/workbench')

# Directories
indir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/'

# Load data
temp_subjs <- read.csv(paste0(indir, 'tabulated/prior_subjects_2025-09-19.csv'))
sesid <- 1
for (i in 1:nrow(temp_subjs)) { 
  subid <- temp_subjs[i, 'subid']
  path <- c(system(paste0('find ', indir, 'surfnet/sub-', subid, '/ses-', sesid,
        ' -name "networks_img.rds"'), intern=TRUE))
  img <- readRDS(path)
  vec <- as.vector(rbind(img$subjNet_mean$data$cortex_left, img$subjNet_mean$data$cortex_right))
  assign(paste0('vec', i), vec)
  print(i)
}

vec_list <- mget(paste0('vec', 1:nrow(temp_subjs)))
xmat <- do.call(rbind, vec_list)

# Center across subjects (rows are subjects)
xmat <- scale(xmat, center = TRUE, scale = FALSE)

# XX' (# of subjects by # of subjects) 
xxtransmat <- tcrossprod(xmat)

# U (# of subjects by # of components)
#Note: U is orthogonal
svd_out <- svd(xxtransmat)
umat <- svd_out$u #correct dimensions

# D (# of components by # of vertices)
A <- t(umat) %*% xxtransmat %*% umat
dmat <- diag(sqrt(diag(A))) #QUESTION: Getting an NA here... What is that about?

# V' (# of vertices by # of vertices)
vtransmat <- 

# Singular values
singvals <- svd_out$d




# svd() notes
       #d: a vector containing the singular values of ‘x’, of length
       #   ‘min(n, p)’, sorted decreasingly.

       #u: a matrix whose columns contain the left singular vectors of
       #   ‘x’, present if ‘nu > 0’.  Dimension ‘c(n, nu)’.

       #v: a matrix whose columns contain the right singular vectors of
       #   ‘x’, present if ‘nv > 0’.  Dimension ‘c(p, nv)’.

       #nu: the number of left singular vectors to be computed.  This
       #   must between ‘0’ and ‘n = nrow(x)’.

       #nv: the number of right singular vectors to be computed.  This
       #   must be between ‘0’ and ‘p = ncol(x)’.













vec_list <- mget(paste0('vec', 1:nrow(temp_subjs)))
xmat <- do.call(rbind, vec_list)

# Center across subjects (rows are subjects)
xmat <- scale(xmat, center = TRUE, scale = FALSE)

# XX'
xxtransmat <- crossprod(xmat) # should be 315622 by 315622... which would be 742.2 GB, so we got to split by network

# U 
