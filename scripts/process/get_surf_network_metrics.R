### This script creates expansion and connectivity estimates
### for the Yeo17 networks. 
### https://github.com/mandymejia/BayesBrainMap
### Yeo networks: https://www.researchgate.net/figure/Network-parcellation-of-Yeos-17-networks-The-17-networks-include-the-following-regions_fig1_352966687#:~:text=The%2017%2Dnetworks%20include%20the%20following%20regions%3A%20N1%3A%20VisCent,N7%3A%20exp_tAttnA%20%2DSalience%2FVentral
###
### Ellyn Butler
### July 16, 2025 - 

# Load libraries
library(BayesBrainMap)
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
sesid = args$sesid #1

print(subid)
print(sesid)

# Set paths
#neurodir <- '~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/'
neurodir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/'
prior <- readRDS(paste0(neurodir, 'prior/prior.rds'))

surfdir <- paste0(neurodir, 'surf/')
outdir <- paste0(neurodir, 'surfnet/')

###### Load Yeo
yeo <- readRDS('/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/prior/GPARC.rds')
yeo <- resample_cifti(yeo, resamp_res = 10000)
yeo <- move_from_mwall(yeo, NA) 

###### Load the cifti
path <- paste0(surfdir, 'sub-', subid, '/ses-', sesid, '/func/sub-', subid, 
        '_ses-', sesid, '_task-chatroom_run-01_space-fsLR_desc-maxpostproc_bold.dscalar.nii')
cii <- read_cifti(path)

###### Single subject template estimation 
print('Single subject map estimation')
networks_img <- BrainMap(cii, prior, tvar_method = 'unbiased', hpf = 0,
                scale = 'local', TR = 2.05, scale_sm_FWHM = 2, GSR = FALSE) 

saveRDS(networks_img, paste0(outdir, 'sub-', subid, '/ses-', sesid, '/networks_img.rds'))

###### Identify areas of engagement and deviation
print('Identify areas of engagement')
network_membership <- engagements(networks_img, z = 3, verbose = TRUE, alpha = 0.01, method_p = 'fdr', type = '>')

saveRDS(network_membership, paste0(outdir, 'sub-', subid, '/ses-', sesid, '/network_membership.rds'))

cii <- move_from_mwall(cii, NA) 

# Get surface area for each vertex in fsLR32k resampled to 10k
fsLR_left <- load_surf('left', name = 'midthickness', resamp_res = 10000) #dim = 10242 3
fsLR_right <- load_surf('right', name = 'midthickness', resamp_res = 10000) #dim = 10242 3
sa_left <- surf_area(fsLR_left)
sa_right <- surf_area(fsLR_right)
sa <- c(sa_left, sa_right)

networks <- c('visuala', 'visualb', 'somatomotora', 'somatomotorb', 'dorsalattentiona',
              'dorsalattentionb', 'saliencea', 'salienceb', 'limbica', 'limbicb',
              'controla', 'controlb', 'controlc', 'defaulta', 'defaultb', 'defaultc',
              'temporalparietal')

pos_eng_cii <- network_membership$engaged > 0
pos_lvl_cii <- networks_img$subjNet_mean # dim(pos_lvl_cii) = 18566
pos_lvl_cii <- move_from_mwall(pos_lvl_cii, NA)

##### Define dataframe
df <- data.frame(subid = subid, sesid = sesid)

for (net1 in 1:17) {
        ###### Check to see if any vertices belong to net1
        mask_pos <- as.matrix(network_membership$engaged)[, net1] > 0
        if (sum(mask_pos) == 0) { next }

        ###### Get the area that each network takes up
        print('Estimate expansion')

        wvec <- as.matrix(pos_eng_cii)[, net1] * as.matrix(pos_lvl_cii)[, net1] * sa
        left_wsum <- sum(wvec[seq(10242)], na.rm=TRUE)
        right_wsum <- sum(wvec[seq(10243, 20484)], na.rm=TRUE)

        sa_nonmed <- sa[!is.na(wvec)] 

        exp_pos <- (left_wsum + right_wsum)/(sum(sa_nonmed)) 
        df[, paste0('exp_', networks[net1], '_pos')] <- exp_pos

        ###### Estimate personalized within network connectivity
        print('Estimate personalized within network connectivity')

        FC_net1_pos <- cor(t(as.matrix(cii)[which(mask_pos),])) #time series engaged locations by all the time points
        FC_pos <- 0
        denom <- 0
        net1_sa <- sa[which(mask_pos)]
        net1_eng <- as.matrix(pos_lvl_cii)[which(mask_pos), net1]
        for (i in 1:ncol(FC_net1_pos)) {
                for (j in (i+1):ncol(FC_net1_pos)) {
                        if (i < ncol(FC_net1_pos)) {
                                FC_pos <- FC_pos + net1_sa[i]*net1_eng[i]*net1_sa[j]*net1_eng[j]*FC_net1_pos[i, j]
                                denom <- denom + net1_sa[i]*net1_eng[i]*net1_sa[j]*net1_eng[j]
                        }
                }
        }
        FC_pos <- FC_pos/denom
        df[, paste0('FC_pers_', networks[net1], '_pos')] <- FC_pos

        ###### Estimate personalized between network connectivity
        print('Estimate personalized between network connectivity')

        for (net2 in (net1+1):17) {
                # Check to make sure some vertices actually belong to net2
                mask_pos2 <- as.matrix(network_membership$engaged)[, net2] > 0 #length 20484

                if (sum(mask_pos2) == 0) { next }
                FC_net1_net2_pos <- cor(t(as.matrix(cii)[which(mask_pos),]), 
                        t(as.matrix(cii)[which(mask_pos2),])) 
                FC_pos <- 0
                denom <- 0
                net2_sa <- sa[which(mask_pos2)]
                net2_eng <- as.matrix(pos_lvl_cii)[which(mask_pos2), net2]
                for (i in 1:nrow(FC_net1_net2_pos)) {
                        for (j in 1:ncol(FC_net1_net2_pos)) {
                                FC_pos <- FC_pos + (net1_sa[i]*net1_eng[i]*net1_sa[j]*net2_eng[j]*FC_net1_net2_pos[i, j])
                                denom <- denom + net1_sa[i]*net1_eng[i]*net1_sa[j]*net2_eng[j]
                        }
                }
                FC_pos <- FC_pos/denom
                df[, paste0('FC_pers_', networks[net1], '_', networks[net2], '_pos')] <- FC_pos
        } 

        ###### Estimate group within network connectivity
        print('Estimate group within network connectivity')

        mask_pos <- as.matrix(yeo) == net1 
        FC_net1_pos <- cor(t(as.matrix(cii)[which(mask_pos),])) 
        FC_pos <- 0
        denom <- 0
        net1_sa <- sa[which(mask_pos)]
        for (i in 1:ncol(FC_net1_pos)) {
                for (j in (i+1):ncol(FC_net1_pos)) {
                        if (i < ncol(FC_net1_pos)) {
                                FC_pos <- FC_pos + net1_sa[i]*net1_sa[j]*FC_net1_pos[i, j]
                                denom <- denom + net1_sa[i]*net1_sa[j]
                        }
                }
        }
        FC_pos <- FC_pos/denom
        df[, paste0('FC_group_', networks[net1], '_pos')] <- FC_pos

        ###### Estimate group between network connectivity
        print('Estimate group between network connectivity')

        for (net2 in (net1+1):17) {
                mask_pos2 <- as.matrix(yeo) == net2
                FC_net1_net2_pos <- cor(t(as.matrix(cii)[which(mask_pos),]), 
                        t(as.matrix(cii)[which(mask_pos2),])) 
                FC_pos <- 0
                denom <- 0
                net2_sa <- sa[which(mask_pos2)]
                for (i in 1:nrow(FC_net1_net2_pos)) {
                        for (j in 1:ncol(FC_net1_net2_pos)) {
                                FC_pos <- FC_pos + (net1_sa[i]*net1_sa[j]*FC_net1_net2_pos[i, j])
                                denom <- denom + net1_sa[i]*net1_sa[j]
                        }
                }
                FC_pos <- FC_pos/denom
                df[, paste0('FC_group_', networks[net1], '_', networks[net2], '_pos')] <- FC_pos
        } #correct up to here now

        ###### Estimate intersection within network connectivity
        print('Estimate intersection within network connectivity')

        mask_pos <- as.matrix(network_membership$engaged)[, net1] > 0
        mask_pos <- mask_pos*as.matrix(yeo) == net1 #intersection mask
        FC_net1_pos <- cor(t(as.matrix(cii)[which(mask_pos),])) #should be 194x194
        FC_pos <- 0
        denom <- 0
        net1_sa <- sa[which(mask_pos)]
        net1_eng <- as.matrix(pos_lvl_cii)[which(mask_pos), net1]
        for (i in 1:ncol(FC_net1_pos)) {
                for (j in (i+1):ncol(FC_net1_pos)) {
                        if (i < ncol(FC_net1_pos)) {
                                FC_pos <- FC_pos + net1_sa[i]*net1_eng[i]*net1_sa[j]*net1_eng[j]*FC_net1_pos[i, j]
                                denom <- denom + net1_sa[i]*net1_eng[i]*net1_sa[j]*net1_eng[j]
                        }
                }
        }
        FC_pos <- FC_pos/denom
        df[, paste0('FC_int_', networks[net1], '_pos')] <- FC_pos #might be correct, but seems too close to pers

        ###### Estimate intersection between network connectivity
        print('Estimate intersection between network connectivity')

        for (net2 in (net1+1):17) {
                # Check to make sure some vertices actually belong to net2
                mask_pos2 <- as.matrix(network_membership$engaged)[, net2] > 0 #length 20484
                mask_pos2 <- mask_pos*as.matrix(yeo) == net2

                if (sum(mask_pos2) == 0) { next }
                FC_net1_net2_pos <- cor(t(as.matrix(cii)[which(mask_pos),]), 
                        t(as.matrix(cii)[which(mask_pos2),])) 
                FC_pos <- 0
                denom <- 0
                net2_sa <- sa[which(mask_pos2)]
                net2_eng <- as.matrix(pos_lvl_cii)[which(mask_pos2), net2]
                for (i in 1:nrow(FC_net1_net2_pos)) {
                        for (j in 1:ncol(FC_net1_net2_pos)) {
                                FC_pos <- FC_pos + (net1_sa[i]*net1_eng[i]*net1_sa[j]*net2_eng[j]*FC_net1_net2_pos[i, j])
                                denom <- denom + net1_sa[i]*net1_eng[i]*net1_sa[j]*net2_eng[j]
                        }
                }
                FC_pos <- FC_pos/denom
                df[, paste0('FC_pers_', networks[net1], '_', networks[net2], '_pos')] <- FC_pos
        }   
}

###### Output the data
print('Output')

write.csv(df, paste0(outdir, 'sub-', subid, '/ses-', sesid, '/sub-', subid, '_ses-', 
                     sesid, '_surf_network_metrics.csv'), row.names = FALSE)
