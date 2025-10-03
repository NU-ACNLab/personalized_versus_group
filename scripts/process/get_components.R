### This script gets spatial components from the 
### engagement maps via singular value decomposition
###
### Ellyn Butler
### October 2, 2025 - 

set.seed(1432)

# Load libraries
library(ggplot2)
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
#Note: # of components = # of subjects here
svd_out <- svd(xxtransmat)
umat <- svd_out$u #correct dimensions

# D (# of components by # of subjects)
A <- t(umat) %*% xxtransmat %*% umat
dmat <- diag(sqrt(diag(A))) #QUESTION: Getting an NA here... What is that about? Result of centering... but we still want to center?

# V' (# of subjects by # of vertices) #QUESTION: How is this useful for visualizing components? Do I have the axes wrong?
vtransmat <- solve(dmat) %*% t(umat) %*% xmat

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

write.csv(df, paste0(indir, 'tabulated/variance_explained_svd.csv'), row.names = FALSE)

plot1 <- ggplot(df, aes(x = component, y = variance_explained)) +
            theme_linedraw() + 
            geom_line() + 
            geom_point(size = 4) +
            xlab('Component') + 
            ylab('Variance Explained') +
            ylim(0, .03)

pdf('/projects/b1108/projects/personalized_versus_group/plots/variance_explained_svd.pdf', width = 8, height = 6) 
plot1
dev.off()

plot2 <- ggplot(df, aes(x = component, y = cumulative_variance_explained)) +
            theme_linedraw() + 
            geom_line() + 
            geom_point(size = 4) +
            xlab('Component') + 
            ylab('Cumulative Variance Explained') +
            ylim(0, 1)

pdf('/projects/b1108/projects/personalized_versus_group/plots/cumulative_variance_explained_svd.pdf', width = 8, height = 6) 
plot2
dev.off()

# Save workspace for future tinkering (delete/modify later)
save.image('/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/tmp/svd.RData')
#load('/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/tmp/svd.RData')


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
