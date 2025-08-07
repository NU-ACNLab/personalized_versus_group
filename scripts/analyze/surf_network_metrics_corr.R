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
df <- read.csv('/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/surf_network_metrics_2025-08-07.csv')

# Make plots
networks <- c('visuala', 'visualb', 'somatomotora', 'somatomotorb', 'dorsalattentiona',
              'dorsalattentionb', 'saliencea', 'salienceb', 'limbica', 'limbicb',
              'controla', 'controlb', 'controlc', 'defaulta', 'defaultb', 'defaultc',
              'temporalparietal')

for (net in networks) {
    netcols <- names(df)[grepl(net, names(df))]
    cormat <- cor(df[, netcols])
}