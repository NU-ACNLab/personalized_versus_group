### This script concatenates the maxpostproc'd ciftis
###
### Ellyn Butler
### September 18, 2025

# Load libraries
library(ciftiTools)
library(argparse)
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

# Set directories
indir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/'

# Combine the ciftis
paths <- system(paste0('find ', indir,'surf/sub-', subid, '/ses-', sesid, 
            '/func -name "*_space-fsLR_desc-maxpostproc_bold.dscalar.nii"'), intern = TRUE)
i = 1
for (path in paths) {
        cii <- read_cifti(path)
        if (i > 1){
                cii <- cbind(cii, get(paste0('cii', i-1))) #not sure cbind is right here
        }
        assign(paste0('cii', i), cii)
        i = i + 1
}

# Write out image
write_cifti(cii, paste0(indir, 'surf/sub-', subid, '/ses-', sesid, '/func/sub-', subid, 
                '_ses-', sesid, '_task-all_space-fsLR_desc-maxpostproc_bold.dscalar.nii'))