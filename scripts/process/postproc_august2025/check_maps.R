### This script checks maps for subjects with a lot of
### missing data
###
### Ellyn Butler
### August 12, 2025

# Load libraries
library(ciftiTools)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(png)
library(grid)
#ciftiTools.setOption('wb_path', '/Applications/workbench')
ciftiTools.setOption('wb_path', '/projects/b1108/software/workbench')

######### 50061 (most missing networks)

basedir <- '/projects/b1108/projects/personalized_versus_group/'
plotdir <- '/projects/b1108/projects/personalized_versus_group/plots/'
#plotdir <- '~/Documents/Northwestern/projects/personalized_versus_group/plots/'

### Load subject
netimg <- readRDS(paste0(basedir, 'data/processed/neuroimaging/surfnet/sub-50061/ses-1/networks_img.rds'))

### Raw
# mean
netimg$subjNet_mean$meta$cifti$names <- c('Visual A', 'Visual B',
    'Somatomotor A', 'Somatomotor B', 'Dorsal Attention A', 
    'Dorsal Attention B', 'Salience Ventral Attention A', 
    'Salience Ventral Attention B', 'Limbic A', 'Limbic B', 'Control A', 
    'Control B', 'Control C', 'Default A', 'Default B', 'Default C', 
    'Temporal Parietal')
plot(netimg$subjNet_mean, fname=paste0(plotdir, 'sub-50061_ses-1_mean'),  idx=1:17)

for (i in 1:17) {
    ICmean <- readPNG(paste0(plotdir, 'sub-50061_ses-1_mean_', netimg$subjNet_mean$meta$cifti$names[i], '.png'))
    assign(paste0('IC', i), ICmean)
}

pdf(paste0(plotdir, 'sub-50061_ses-1_mean.pdf'), width = 9, height = 10) 
grid.arrange(rasterGrob(IC1), rasterGrob(IC2), rasterGrob(IC3), rasterGrob(IC4), 
             rasterGrob(IC5), rasterGrob(IC6), rasterGrob(IC7), rasterGrob(IC8), 
             rasterGrob(IC9), rasterGrob(IC10), rasterGrob(IC11), 
             rasterGrob(IC12), rasterGrob(IC13), rasterGrob(IC14), 
             rasterGrob(IC15), rasterGrob(IC16), rasterGrob(IC17), ncol = 4)
dev.off()

# variance
netimg$subjNet_se$meta$cifti$names <- c('Visual A', 'Visual B',
    'Somatomotor A', 'Somatomotor B', 'Dorsal Attention A', 
    'Dorsal Attention B', 'Salience Ventral Attention A', 
    'Salience Ventral Attention B', 'Limbic A', 'Limbic B', 'Control A', 
    'Control B', 'Control C', 'Default A', 'Default B', 'Default C', 
    'Temporal Parietal')
plot(netimg$subjNet_se, fname=paste0(plotdir, 'sub-50061_ses-1_se'),  idx=1:17)

for (i in 1:17) {
    ICse <- readPNG(paste0(plotdir, 'sub-50061_ses-1_se_', netimg$subjNet_se$meta$cifti$names[i], '.png'))
    assign(paste0('IC', i), ICse)
}

pdf(paste0(plotdir, 'sub-50061_ses-1_se.pdf'), width = 9, height = 10) 
grid.arrange(rasterGrob(IC1), rasterGrob(IC2), rasterGrob(IC3), rasterGrob(IC4), 
             rasterGrob(IC5), rasterGrob(IC6), rasterGrob(IC7), rasterGrob(IC8), 
             rasterGrob(IC9), rasterGrob(IC10), rasterGrob(IC11), 
             rasterGrob(IC12), rasterGrob(IC13), rasterGrob(IC14), 
             rasterGrob(IC15), rasterGrob(IC16), rasterGrob(IC17), ncol = 4)
dev.off()

### Thresholded
# mean
netimg_thresh <- netimg
netimg_thresh$subjNet_mean$data$cortex_left <- netimg_thresh$subjNet_mean$data$cortex_left*(netimg_thresh$subjNet_mean$data$cortex_left > .2)
netimg_thresh$subjNet_mean$data$cortex_right <- netimg_thresh$subjNet_mean$data$cortex_right*(netimg_thresh$subjNet_mean$data$cortex_right > .2)
plot(netimg_thresh$subjNet_mean, fname=paste0(plotdir, 'sub-50061_ses-1_mean_thresh'),  idx=1:17)

for (i in 1:17) {
    ICmean <- readPNG(paste0(plotdir, 'sub-50061_ses-1_mean_thresh_', netimg_thresh$subjNet_mean$meta$cifti$names[i], '.png'))
    assign(paste0('IC', i), ICmean)
}

pdf(paste0(plotdir, 'sub-50061_ses-1_mean_thresh.pdf'), width = 9, height = 10) 
grid.arrange(rasterGrob(IC1), rasterGrob(IC2), rasterGrob(IC3), rasterGrob(IC4), 
             rasterGrob(IC5), rasterGrob(IC6), rasterGrob(IC7), rasterGrob(IC8), 
             rasterGrob(IC9), rasterGrob(IC10), rasterGrob(IC11), 
             rasterGrob(IC12), rasterGrob(IC13), rasterGrob(IC14), 
             rasterGrob(IC15), rasterGrob(IC16), rasterGrob(IC17), ncol = 4)
dev.off()

######### 50002 (no missing networks)

### Load subject
netimg <- readRDS(paste0(basedir, 'data/processed/neuroimaging/surfnet/sub-50002/ses-1/networks_img.rds'))

### Raw
# mean
netimg$subjNet_mean$meta$cifti$names <- c('Visual A', 'Visual B',
    'Somatomotor A', 'Somatomotor B', 'Dorsal Attention A', 
    'Dorsal Attention B', 'Salience Ventral Attention A', 
    'Salience Ventral Attention B', 'Limbic A', 'Limbic B', 'Control A', 
    'Control B', 'Control C', 'Default A', 'Default B', 'Default C', 
    'Temporal Parietal')
plot(netimg$subjNet_mean, fname=paste0(plotdir, 'sub-50002_ses-1_mean'),  idx=1:17)

for (i in 1:17) {
    ICmean <- readPNG(paste0(plotdir, 'sub-50002_ses-1_mean_', netimg$subjNet_mean$meta$cifti$names[i], '.png'))
    assign(paste0('IC', i), ICmean)
}

pdf(paste0(plotdir, 'sub-50002_ses-1_mean.pdf'), width = 9, height = 10) 
grid.arrange(rasterGrob(IC1), rasterGrob(IC2), rasterGrob(IC3), rasterGrob(IC4), 
             rasterGrob(IC5), rasterGrob(IC6), rasterGrob(IC7), rasterGrob(IC8), 
             rasterGrob(IC9), rasterGrob(IC10), rasterGrob(IC11), 
             rasterGrob(IC12), rasterGrob(IC13), rasterGrob(IC14), 
             rasterGrob(IC15), rasterGrob(IC16), rasterGrob(IC17), ncol = 4)
dev.off()

# variance
netimg$subjNet_se$meta$cifti$names <- c('Visual A', 'Visual B',
    'Somatomotor A', 'Somatomotor B', 'Dorsal Attention A', 
    'Dorsal Attention B', 'Salience Ventral Attention A', 
    'Salience Ventral Attention B', 'Limbic A', 'Limbic B', 'Control A', 
    'Control B', 'Control C', 'Default A', 'Default B', 'Default C', 
    'Temporal Parietal')
plot(netimg$subjNet_se, fname=paste0(plotdir, 'sub-50002_ses-1_se'),  idx=1:17)

for (i in 1:17) {
    ICse <- readPNG(paste0(plotdir, 'sub-50002_ses-1_se_', netimg$subjNet_se$meta$cifti$names[i], '.png'))
    assign(paste0('IC', i), ICse)
}

pdf(paste0(plotdir, 'sub-50002_ses-1_se.pdf'), width = 9, height = 10) 
grid.arrange(rasterGrob(IC1), rasterGrob(IC2), rasterGrob(IC3), rasterGrob(IC4), 
             rasterGrob(IC5), rasterGrob(IC6), rasterGrob(IC7), rasterGrob(IC8), 
             rasterGrob(IC9), rasterGrob(IC10), rasterGrob(IC11), 
             rasterGrob(IC12), rasterGrob(IC13), rasterGrob(IC14), 
             rasterGrob(IC15), rasterGrob(IC16), rasterGrob(IC17), ncol = 4)
dev.off()

### Thresholded
# mean
netimg_thresh <- netimg
netimg_thresh$subjNet_mean$data$cortex_left <- netimg_thresh$subjNet_mean$data$cortex_left*(netimg_thresh$subjNet_mean$data$cortex_left > .2)
netimg_thresh$subjNet_mean$data$cortex_right <- netimg_thresh$subjNet_mean$data$cortex_right*(netimg_thresh$subjNet_mean$data$cortex_right > .2)
plot(netimg_thresh$subjNet_mean, fname=paste0(plotdir, 'sub-50002_ses-1_mean_thresh'),  idx=1:17)

for (i in 1:17) {
    ICmean <- readPNG(paste0(plotdir, 'sub-50002_ses-1_mean_thresh_', netimg_thresh$subjNet_mean$meta$cifti$names[i], '.png'))
    assign(paste0('IC', i), ICmean)
}

pdf(paste0(plotdir, 'sub-50002_ses-1_mean_thresh.pdf'), width = 9, height = 10) 
grid.arrange(rasterGrob(IC1), rasterGrob(IC2), rasterGrob(IC3), rasterGrob(IC4), 
             rasterGrob(IC5), rasterGrob(IC6), rasterGrob(IC7), rasterGrob(IC8), 
             rasterGrob(IC9), rasterGrob(IC10), rasterGrob(IC11), 
             rasterGrob(IC12), rasterGrob(IC13), rasterGrob(IC14), 
             rasterGrob(IC15), rasterGrob(IC16), rasterGrob(IC17), ncol = 4)
dev.off()
