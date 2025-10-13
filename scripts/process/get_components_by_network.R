### This script gets spatial components from the 
### engagement maps via singular value decomposition
###
### Ellyn Butler
### October 2, 2025 - October 6, 2025

set.seed(1432)

# Load libraries
library(ggplot2)
library(ciftiTools)
library(ggpubr)
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
  vec_list <- mget(paste0('vec_i', 1:nrow(temp_subjs), '_j', j))
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

  # D*V' (# of components by # of vertices)
  dvtransmat <- dmat %*% vtransmat
  write.csv(dvtransmat, paste0(indir, 'components/', netnames[j], '.csv'), row.names = FALSE)

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

  write.csv(df, paste0(indir, 'tabulated/variance_explained_j', j, '_svd.csv'), row.names = FALSE)

  plot1 <- ggplot(df, aes(x = component, y = variance_explained)) +
              theme_linedraw() + 
              geom_line() + 
              #geom_point(size = 4) +
              xlab('Component') + 
              ylab('Variance Explained') +
              ggtitle(netnames[j]) + 
              ylim(0, .20)

  assign(paste0('plot1_j', j), plot1)

  plot2 <- ggplot(df, aes(x = component, y = cumulative_variance_explained)) +
              theme_linedraw() + 
              geom_line() + 
              #geom_point(size = 4) +
              xlab('Component') + 
              ylab('Cumulative Variance Explained') +
              ggtitle(netnames[j]) + 
              ylim(0, 1)

  assign(paste0('plot2_j', j), plot2)
}

pdf(paste0('/projects/b1108/projects/personalized_versus_group/plots/variance_explained_by_network_svd.pdf'), width = 20, height = 16) 
ggarrange(plot1_j1, plot1_j2, plot1_j3, plot1_j4, plot1_j5, plot1_j6, 
          plot1_j7, plot1_j8, plot1_j9, plot1_j10, plot1_j11, plot1_j12, 
          plot1_j13, plot1_j14, plot1_j15, plot1_j16, plot1_j17)
dev.off()

pdf(paste0('/projects/b1108/projects/personalized_versus_group/plots/cumulative_variance_explained_by_network_svd.pdf'), width = 20, height = 16) 
ggarrange(plot2_j1, plot2_j2, plot2_j3, plot2_j4, plot2_j5, plot2_j6, 
          plot2_j7, plot2_j8, plot2_j9, plot2_j10, plot2_j11, plot2_j12, 
          plot2_j13, plot2_j14, plot2_j15, plot2_j16, plot2_j17)
dev.off()


# Save workspace for future tinkering (delete/modify later)
save.image('/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/tmp/svd_by_network.RData')
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


