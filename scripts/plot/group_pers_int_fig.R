### This script plots group, personalized, and intersection
### maps for two networks
###
### Ellyn Butler
### August 21, 2025 - December 4, 2025

# Load libraries
library(ciftiTools)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(png)
library(grid)

ciftiTools.setOption('wb_path', '/Applications/workbench')
#ciftiTools.setOption('wb_path', '/projects/b1108/software/workbench')

basedir <- '~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/'

### A. Group
yeo <- readRDS(paste0(basedir, '/prior/GPARC.rds'))
yeo <- resample_cifti(yeo, resamp_res = 10000)

yeo$data$cortex_left[!(yeo$data$cortex_left %in% c(4, 14))] <- NA
yeo$data$cortex_right[!(yeo$data$cortex_right %in% c(4, 14))] <- NA

view_xifti_surface(yeo)
yeo <- move_from_mwall(yeo, NA) 

### B. Personalized
# Load subject
netimg <- readRDS(paste0(basedir, '/surfnet/sub-50002/ses-1/networks_img.rds'))
netmem <- readRDS(paste0(basedir, '/surfnet/sub-50002/ses-1/network_membership_z1.rds'))

# Modify the xifti for viewing pleasure
netimg$subjNet_mean <- move_from_mwall(netimg$subjNet_mean, NA) 
netimg$subjNet_mean <- netimg$subjNet_mean*netmem$engaged

netimg$subjNet_mean$data$cortex_left[netimg$subjNet_mean$data$cortex_left == 0] <- NA
netimg$subjNet_mean$data$cortex_right[netimg$subjNet_mean$data$cortex_right == 0] <- NA

# Default A
view_xifti_surface(netimg$subjNet_mean, idx = 14, zlim = c(0, .8), 
    fname = '~/Documents/Northwestern/projects/personalized_versus_group/plots/sub-50002_defaultA_pers', 
    legend_embed = FALSE)

# Somatomotor B
view_xifti_surface(netimg$subjNet_mean, idx = 4, zlim = c(0, .8), 
    fname = '~/Documents/Northwestern/projects/personalized_versus_group/plots/sub-50002_somatomotorB_pers', 
    legend_embed = FALSE)

### C. Intersection
# Default A
netimg_da <- netimg
netimg_da$subjNet_mean$data$cortex_left[, 14] <- netimg_da$subjNet_mean$data$cortex_left[,14]*(yeo$data$cortex_left == 14)
netimg_da$subjNet_mean$data$cortex_right[, 14] <- netimg_da$subjNet_mean$data$cortex_right[,14]*(yeo$data$cortex_right == 14)

view_xifti_surface(netimg_da$subjNet_mean, idx = 14, zlim = c(0, .8), 
    fname = '~/Documents/Northwestern/projects/personalized_versus_group/plots/sub-50002_defaultA_int', 
    legend_embed = FALSE)

# Somatomotor B
netimg_sb <- netimg
netimg_sb$subjNet_mean$data$cortex_left[, 4] <- netimg_sb$subjNet_mean$data$cortex_left[,4]*(yeo$data$cortex_left == 4)
netimg_sb$subjNet_mean$data$cortex_right[, 4] <- netimg_sb$subjNet_mean$data$cortex_right[,4]*(yeo$data$cortex_right == 4)

view_xifti_surface(netimg_sb$subjNet_mean, idx = 4, zlim = c(0, .8), 
    fname = '~/Documents/Northwestern/projects/personalized_versus_group/plots/sub-50002_somatomotorB_int', 
    legend_embed = FALSE)

