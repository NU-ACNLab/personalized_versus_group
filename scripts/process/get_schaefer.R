### This script quantifies connectivity within
### the Schaefer 400 atlas
### 
### Ellyn Butler
### December 16, 2025

# Load libraries
library(ciftiTools)
library(dplyr)
library(argparse)
library(xtranat)
#ciftiTools.setOption('wb_path', '/Applications/workbench')
ciftiTools.setOption('wb_path', '/projects/b1108/software/workbench')

# Parse command line arguments
parser <- ArgumentParser()
parser$add_argument('-s', '--subid', type='character', help='Subject Identifier')
parser$add_argument('-e', '--sesid', type='character', help='Session Identifier')

args <- parser$parse_args()

subid = args$subid # 50064
sesid = args$sesid # 1

print(subid)
print(sesid)

# Set paths
#neurodir <- '~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/'
neurodir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/'
prior <- readRDS(paste0(neurodir, 'prior/prior_task-all.rds'))

surfdir <- paste0(neurodir, 'surf/')
outdir <- paste0(neurodir, 'surfnet/')

###### Load Schaefer

###### Load the ciftis
path <- paste0(surfdir, 'sub-', subid, '/ses-', sesid, '/func/sub-', subid, 
        '_ses-', sesid, '_task-all_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
cii <- read_cifti(path)

###### Get connectivity
ParcMat <- load_parc('Schaefer_400')
ParcMat <- resample_cifti(ParcMat, resamp_res = 10000)

# Get the medial wall out so that ParcMat and cii are the same spatial dimensions
cii <- move_from_mwall(cii, 0)

# Mask for medial wall (includes vertices from both medial walls)
cii_mwall <- rowSums(as.matrix(cii) != 0) == 0
ParcMat_mwall <- c(as.matrix(ParcMat) == 0)
mwall <- cii_mwall | ParcMat_mwall

# Turn the cifti into a matrix object so you can use matrix operations on it
cii <- as.matrix(cii)

# Mask out medial wall for cii 
cii <- cii[!(mwall),]

# Create a matrix of indicators out of ParcMat
v <- as.integer(c(ParcMat$data$cortex_left, ParcMat$data$cortex_right))  # ensure integer
v <- v[!(mwall)]
vals <- 1:400
mat <- outer(v, vals, `==`)

# Parcellate
parced <- t(cii - rowMeans(cii)) %*% mat 
# The format of as.matrix(ParcMat) needs to change so that it is 20484 by 401 (number of parcel values), I believe

# Get FC
FC <- cor(parced)