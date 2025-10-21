### This script plots the spatial components from the 
### engagement maps via singular value decomposition
### on each network separately on a single plot
###
### Ellyn Butler
### October 20, 2025

set.seed(1432)

# Load libraries
library(ggplot2)
library(ggpubr)

# Directories
indir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/'

# Load data
netnames <- c('VisCent', 'VisCeri', 'SomMotA', 'SomMotB', 'DorsAttnA',
              'DorsAttnB', 'SalVenAttnA', 'SalVenAttnB', 'LimbicA',
              'LimbicB', 'ContA', 'ContB', 'ContC', 'DefaultA',
              'DefaultB', 'DefaultC', 'TempPar')

for (j in 1:length(netnames)) {
  df <- read.csv(paste0(indir, 'tabulated/variance_explained_thresholded_j', j, '_svd.csv'))
  df$Network <- netnames[j]
  assign(paste0('df', j), df)
  df10 <- df[1:10,]
  assign(paste0('df10_', j), df10)
}

colors <- c('black', 'black', 'blue', 'blue', 'gold', 'gold', 
            'red4', 'red4', 'darkseagreen2', 'darkseagreen2', 'steelblue1', 
            'steelblue1', 'steelblue1', 'peachpuff3', 'peachpuff3', 'peachpuff3', 
            'springgreen3')

# All components
df <- do.call(rbind, mget(paste0('df', 1:17)))
df$Network <- ordered(df$Network, unique(df$Network))

plot1 <- ggplot(df, aes(x = component, y = variance_explained, color = Network)) +
              theme_linedraw() + 
              geom_line() + 
              xlab('Component') + 
              ylab('Variance Explained') +
              scale_color_manual(values = colors) +
              ylim(0, .15)

plot2 <- ggplot(df, aes(x = component, y = cumulative_variance_explained, color = Network)) +
              theme_linedraw() + 
              geom_line() + 
              xlab('Component') + 
              ylab('Cumulative Variance Explained') +
              scale_color_manual(values = colors) +
              ylim(0, 1)



pdf(paste0('/projects/b1108/projects/personalized_versus_group/plots/combined_variance_explained_thresholded_by_network_svd.pdf'), width = 8, height = 6) 
plot1
dev.off()

pdf(paste0('/projects/b1108/projects/personalized_versus_group/plots/combined_cumulative_variance_explained_thresholded_by_network_svd.pdf'), width = 8, height = 6) 
plot2
dev.off()

# First 10 components
df10 <- do.call(rbind, mget(paste0('df10_', 1:17)))
df10$Network <- ordered(df10$Network, unique(df10$Network))

plot1 <- ggplot(df10, aes(x = component, y = variance_explained, color = Network)) +
              theme_linedraw() + 
              geom_line() + 
              xlab('Component') + 
              ylab('Variance Explained') +
              scale_color_manual(values = colors) +
              ylim(0, .25)

plot2 <- ggplot(df10, aes(x = component, y = cumulative_variance_explained, color = Network)) +
              theme_linedraw() + 
              geom_line() + 
              xlab('Component') + 
              ylab('Cumulative Variance Explained') +
              scale_color_manual(values = colors) +
              ylim(0, .35)



pdf(paste0('/projects/b1108/projects/personalized_versus_group/plots/combined_variance_explained_thresholded_by_network_svd_10.pdf'), width = 6, height = 6) 
plot1
dev.off()

pdf(paste0('/projects/b1108/projects/personalized_versus_group/plots/combined_cumulative_variance_explained_thresholded_by_network_svd_10.pdf'), width = 6, height = 6) 
plot2
dev.off()
