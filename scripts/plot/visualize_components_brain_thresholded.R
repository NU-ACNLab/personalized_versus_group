### This script plots the first few components for
### each network from the SVDs to determine how many
### don't just look like noise
###
### Ellyn Butler
### October 15, 2025

set.seed(1432)

# Load libraries
library(ggplot2)
library(ciftiTools)
library(ggpubr)
library(gridExtra)
library(png)
library(grid)
ciftiTools.setOption('wb_path', '/projects/b1108/software/workbench')

# Directories
indir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/'
outdir <- '/projects/b1108/projects/personalized_versus_group/plots/'

# Load one subject for data format
temp_subjs <- read.csv(paste0(indir, 'tabulated/prior_subjects_2025-09-19.csv'))
sesid <- 1
i=1
subid <- temp_subjs[i, 'subid']
path <- c(system(paste0('find ', indir, 'surfnet/sub-', subid, '/ses-', sesid,
          ' -name "networks_img.rds"'), intern=TRUE))
img <- readRDS(path)

# Make plots
netnames <- c('VisCent', 'VisCeri', 'SomMotA', 'SomMotB', 'DorsAttnA',
              'DorsAttnB', 'SalVenAttnA', 'SalVenAttnB', 'LimbicA',
              'LimbicB', 'ContA', 'ContB', 'ContC', 'DefaultA',
              'DefaultB', 'DefaultC', 'TempPar')
for (j in 1:17) {
    components <- read.csv(paste0(indir, 'components/', netnames[j], '_thresholded.csv'))
    img$subjNet_mean$data$cortex_left <- t(components[1:17, 1:nrow(img$subjNet_mean$data$cortex_left)])
    img$subjNet_mean$data$cortex_right <- t(components[1:17, (nrow(img$subjNet_mean$data$cortex_left) + 1):ncol(components)])
    img$subjNet_mean$meta$cifti$names <- paste0('Component ', 1:17)
    print(plot(img$subjNet_mean, idx = 1:17, fname = paste0(outdir, netnames[j], '_thresholded')))
}

for (j in 1:17) {
    for (i in 1:17) {
        weights <- readPNG(paste0(outdir, netnames[j], '_thresholded_', img$subjNet_mean$meta$cifti$names[i], '.png'))
        assign(paste0('w', i), weights)
    }
    pdf(paste0(outdir, netnames[j], '_components_thresholded.pdf'), width = 7, height = 12)
    print(grid.arrange(rasterGrob(w1), rasterGrob(w2), rasterGrob(w3), rasterGrob(w4), 
                rasterGrob(w5), rasterGrob(w6), rasterGrob(w7), rasterGrob(w8), 
                rasterGrob(w9), rasterGrob(w10), rasterGrob(w11), 
                rasterGrob(w12), rasterGrob(w13), rasterGrob(w14), 
                rasterGrob(w15), rasterGrob(w16), rasterGrob(w17), ncol = 3))
    dev.off()
}
