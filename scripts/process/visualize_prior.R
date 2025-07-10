### This script plots the ICs for the 17 networks, 
### with the limits set by the network, not by
### the processing method
###
### Ellyn Butler
### July 10, 2025

# Load libraries
library(BayesBrainMap)
library(ciftiTools)
library(png)
library(grid)
library(gridExtra)
ciftiTools.setOption('wb_path', '/projects/b1108/software/workbench')
#ciftiTools.setOption('wb_path', '/Applications/workbench')

# Set paths
#neurodir <- '~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/'
neurodir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/'
#plotdir <- '~/Documents/Northwestern/projects/personalized_versus_group/plots/'
plotdir <- '/projects/b1108/projects/personalized_versus_group/plots/'

# Load prior
prior <- readRDS(paste0(neurodir, 'prior/prior.rds'))
export_prior(prior, out_fname = paste0(neurodir, 'prior/prior'))

mean_prior <- read_cifti(paste0(neurodir, 'prior/prior_mean.dscalar.nii'))
var_prior <- read_cifti(paste0(neurodir, 'prior/prior_var.dscalar.nii'))

# Define ICs
for (i in 1:17) {
    mt <- select_xifti(mean_prior, i)
    vt <- select_xifti(var_prior, i)
    assign(paste0('IC', i, '_mean'), mt) 
    assign(paste0('IC', i, '_var'), vt) 
}

##### Means
# Export plots
for (i in 1:17) {
    plot(get(paste0('IC', i, '_mean')), fname=paste0(plotdir, 'IC', i, '_mean'))
}

# Load plots
for (i in 1:17) {
    mt <- get(paste0('IC', i, '_mean'))
    plott <- readPNG(paste0(plotdir, 'IC', i, '_mean.png'))
    assign(mt$meta$cifti$names, plott)
}

pdf(paste0(plotdir, 'IC_mean.pdf'), width = 9, height = 10)
grid.arrange(arrangeGrob(rasterGrob(VisCent), top = 'VisCent'), 
             arrangeGrob(rasterGrob(VisPeri), top = 'VisPeri'), 
             arrangeGrob(rasterGrob(SomMotA), top = 'SomMotA'), 
             arrangeGrob(rasterGrob(SomMotB), top = 'SomMotB'), 
             arrangeGrob(rasterGrob(DorsAttnA), top = 'DorsAttnA'), 
             arrangeGrob(rasterGrob(DorsAttnB), top = 'DorsAttnB'), 
             arrangeGrob(rasterGrob(SalVentAttnA), top = 'SalVentAttnA'), 
             arrangeGrob(rasterGrob(SalVentAttnB), top = 'SalVenAttnB'), 
             arrangeGrob(rasterGrob(LimbicA), top = 'LimbicA'), 
             arrangeGrob(rasterGrob(LimbicB), top = 'LimbicB'), 
             arrangeGrob(rasterGrob(ContA), top = 'ContA'), 
             arrangeGrob(rasterGrob(ContB), top = 'ContB'), 
             arrangeGrob(rasterGrob(ContC), top = 'ContC'), 
             arrangeGrob(rasterGrob(DefaultA), top = 'DefaultA'), 
             arrangeGrob(rasterGrob(DefaultB), top = 'DefaultB'),
             arrangeGrob(rasterGrob(DefaultC), top = 'DefaultC'), 
             arrangeGrob(rasterGrob(TempPar), top = 'TempPar'), ncol = 4, nrow = 5)
dev.off()

 
##### Variances
# Export plots
for (i in 1:17) {
    plot(get(paste0('IC', i, '_var')), fname=paste0(plotdir, 'IC', i, '_var'))
}

# Load plots
for (i in 1:17) {
    vt <- get(paste0('IC', i, '_var'))
    plott <- readPNG(paste0(plotdir, 'IC', i, '_var.png'))
    assign(vt$meta$cifti$names, plott)
}

pdf(paste0(plotdir, 'IC_var.pdf'), width = 9, height = 10)
grid.arrange(arrangeGrob(rasterGrob(VisCent), top = 'VisCent'), 
             arrangeGrob(rasterGrob(VisPeri), top = 'VisPeri'), 
             arrangeGrob(rasterGrob(SomMotA), top = 'SomMotA'), 
             arrangeGrob(rasterGrob(SomMotB), top = 'SomMotB'), 
             arrangeGrob(rasterGrob(DorsAttnA), top = 'DorsAttnA'), 
             arrangeGrob(rasterGrob(DorsAttnB), top = 'DorsAttnB'), 
             arrangeGrob(rasterGrob(SalVentAttnA), top = 'SalVentAttnA'), 
             arrangeGrob(rasterGrob(SalVentAttnB), top = 'SalVenAttnB'), 
             arrangeGrob(rasterGrob(LimbicA), top = 'LimbicA'), 
             arrangeGrob(rasterGrob(LimbicB), top = 'LimbicB'), 
             arrangeGrob(rasterGrob(ContA), top = 'ContA'), 
             arrangeGrob(rasterGrob(ContB), top = 'ContB'), 
             arrangeGrob(rasterGrob(ContC), top = 'ContC'), 
             arrangeGrob(rasterGrob(DefaultA), top = 'DefaultA'), 
             arrangeGrob(rasterGrob(DefaultB), top = 'DefaultB'),
             arrangeGrob(rasterGrob(DefaultC), top = 'DefaultC'), 
             arrangeGrob(rasterGrob(TempPar), top = 'TempPar'), ncol = 4, nrow = 5)
dev.off()

