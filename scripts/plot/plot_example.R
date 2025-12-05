### This script plots the subject maps and engagements for a particular
### subject for the personalized networks approach figure
###
### Ellyn Butler
### December 4, 2025 - December 5, 2025

library(ciftiTools)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(png)
library(grid)
ciftiTools.setOption('wb_path', '/Applications/workbench')
#ciftiTools.setOption('wb_path', '/projects/b1108/software/workbench')

##### Plot 
eng <- readRDS('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/surfnet/sub-50002/ses-1/network_membership_z1.rds')

img <- readRDS('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/surfnet/sub-50002/ses-1/networks_img.rds')

img$subjNet_mean <- move_from_mwall(img$subjNet_mean)
view_xifti_surface(img$subjNet_mean, idx = 14, zlim = c(-.2, 0.2))
view_xifti_surface(img$subjNet_se, idx = 14, zlim = c(0, 0.08))

view_xifti_surface(eng$engaged, idx = 14)
