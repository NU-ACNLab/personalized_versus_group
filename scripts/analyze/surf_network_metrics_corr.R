### This script creates correlation matrices between
### the different surface network metrics as a sanity
### check
###
### Ellyn Butler
### August 7, 2025

# Load libraries
library(ggplot2)
library(ggcorrplot)
library(ggpubr)

# Load data
df <- read.csv('/Users/flutist4129/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/surf_network_metrics_2025-08-07.csv')

# Define functions
get_p_matrix <- function(df) {
  n <- ncol(df)
  p_mat <- matrix(NA, n, n)
  colnames(p_mat) <- rownames(p_mat) <- colnames(df)
  
  for (i in 1:n) {
    for (j in 1:n) {
      p_mat[i, j] <- cor.test(df[[i]], df[[j]])$p.value
    }
  }
  return(p_mat)
}

# Make plots
networks <- c('visuala', 'visualb', 'somatomotora', 'somatomotorb', 'dorsalattentiona',
              'dorsalattentionb', 'saliencea', 'salienceb', 'limbica', 'limbicb',
              'controla', 'controlb', 'controlc', 'defaulta', 'defaultb', 'defaultc',
              'temporalparietal')

for (net in networks) {
    netcols <- names(df)[grepl(net, names(df))]
    cormat <- round(cor(df[, netcols], use = 'pairwise.complete.obs'), 2)
    splits <- strsplit(rownames(cormat), '_')
    for (i in 1:length(splits)) {
        if (length(splits[[i]]) == 5) {
            colnames(cormat)[i] <- paste0(splits[[i]][2], '_', splits[[i]][4])
            rownames(cormat)[i] <- paste0(splits[[i]][2], '_', splits[[i]][4])
        } else if (length(splits[[i]]) == 4) {
            colnames(cormat)[i] <- splits[[i]][2]
            rownames(cormat)[i] <- splits[[i]][2]
        } else {
            colnames(cormat)[i] <- 'expansion'
            rownames(cormat)[i] <- 'expansion'
        }
    }
    #p_matrix <- get_p_matrix(df[, netcols])
    corplot <- ggcorrplot(cormat, lab = TRUE, lab_size = 2) + ggtitle(net)

    assign(paste0(net, '_cormat'), cormat)
    assign(paste0(net, '_corplot'), corplot)

    # Export plot
    png(paste0('/Users/flutist4129/Documents/Northwestern/projects/personalized_versus_group/plots/correlations/corrplot_', net, '.png'), width=10000, height=9000, res=1000)
    print(corplot)
    dev.off()
} # August 7, 2025: Looks off. Try fixing data first