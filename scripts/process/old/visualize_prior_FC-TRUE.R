### This script plots the ICs for the 17 networks, 
### with the limits set by the network, not by
### the processing method
###
### Ellyn Butler
### July 10, 2025 - September 25, 2025

# Load libraries
library(BayesBrainMap)
library(ciftiTools)
library(png)
library(grid)
library(gridExtra)
library(fMRItools)
#ciftiTools.setOption('wb_path', '/projects/b1108/software/workbench')
ciftiTools.setOption('wb_path', '/Applications/workbench')

# Set paths
neurodir <- '~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/'
#neurodir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/'
plotdir <- '~/Documents/Northwestern/projects/personalized_versus_group/plots/'
#plotdir <- '/projects/b1108/projects/personalized_versus_group/plots/'

# Load prior
prior <- readRDS(paste0(neurodir, 'prior/prior_task-all_FC-TRUE.rds'))

plot_FC_gg(prior$prior$FC$mean_empirical)

plot_FC_gg(prior$prior$FC_Chol$FC_samp_mean)