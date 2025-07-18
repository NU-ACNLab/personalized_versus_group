### This script creates expansion and connectivity estimates
### for the Yeo17 networks. 
### https://github.com/mandymejia/BayesBrainMap
### Yeo networks: https://www.researchgate.net/figure/Network-parcellation-of-Yeos-17-networks-The-17-networks-include-the-following-regions_fig1_352966687#:~:text=The%2017%2Dnetworks%20include%20the%20following%20regions%3A%20N1%3A%20VisCent,N7%3A%20exp_tAttnA%20%2DSalience%2FVentral
###
### Ellyn Butler
### July 16, 2025

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
network_membership <- engagements(networks_img, z = 3, verbose = TRUE, method_p = 'fdr', type = '>')

saveRDS(network_membership, paste0(outdir, 'sub-', subid, '/ses-', sesid, '/network_membership.rds'))

cii <- move_from_mwall(cii, NA) #TBD where to put this exactly

# Get surface area for each vertex in fsLR32k resampled to 10k
fsLR <- load_surf(name = 'midthickness', resamp_res = 10000) #dim = 10242 3
sa <- surf_area(fsLR) #vector of length 10242... shouldn't it be twice as long? what with both hemispheres?
                      # Or is it equal for each hemisphere?

networks <- c('visuala', 'visualb', 'somatomotora', 'somatomotorb', 'dorsalattentiona',
              'dorsalattentionb', 'saliencea', 'salienceb', 'limbica', 'limbicb',
              'controla', 'controlb', 'controlc', 'defaulta', 'defaultb', 'defaultc',
              'temporalparietal')
for (net1 in 1:17) {
        ###### Get the area that each network takes up
        print('Expansion')

        pos_eng_cii <- network_membership$engaged > 0
        pos_lvl_cii <- networks_img$subjNet_mean
        pos_lvl_cii <- move_from_mwall(pos_lvl_cii, NA)

        wvec <- as.matrix(pos_eng_cii)[, net1] * as.matrix(pos_lvl_cii)[, net1] * sa
        left_wsum <- sum(wvec[seq(10242)], na.rm=TRUE)
        right_wsum <- sum(wvec[seq(10243, 20484)], na.rm=TRUE)

        sa_nonmed <- c(sa, sa)[!is.na(wvec)] #!is.na(wvec) is 9283 though

        exp_pos <- (left_wsum + right_wsum)/(sum(nonmed_sa)) #want everything to be 9282 so the sum(sa) makes sense... don't want to be including sa of medial wall vertices
        #exp_pos <- (sum(c(network_membership$engaged$data[[1]][, net]) == 1, na.rm = TRUE) + sum(c(network_membership$engaged$data[[2]][, net]) == 1, na.rm = TRUE))/(nrow(network_membership$engaged$data[[1]]) + nrow(network_membership$engaged$data[[2]]))
        assign(paste0('exp_', networks[net1], '_pos'), exp_pos)

        ###### Estimate personalized within network connectivity - July 17, 2025: This chunk is working
        print('Connectivity')

        mask_pos <- as.matrix(network_membership$engaged)[, net1] > 0 #length 20484
        FC_net1_pos <- cor(t(as.matrix(cii)[mask_pos & complete.cases(as.matrix(cii)),])) #time series engaged locations by all the time points
        FC_pos <- 0
        sas <- c(sa, sa)
        net1_sa <- sas[as.matrix(pos_eng_cii)[, net1]] #Why are they all 5.33507? There should only be two like this... sum(sas == net1_sa[1])
        net1_eng <- pos_lvl_cii[as.matrix(pos_eng_cii)[, net1]]
        for (i in 1:ncol(FC_mat_pos)) {
                for (j in (i+1):ncol(FC_mat_pos)) {
                        if (i < ncol(FC_mat_pos)) {
                                FC_pos <- FC_pos + (net1_sa[i]*net1_eng[i]*net1_sa[j]*net1_eng[j]*FC_net1_pos[i, j])/(this_sa[i] + this_sa[j])
                        }
                }
        }
        assign(paste0('FC_pers_', networks[net], '_pos'), FC_pos)

        ###### Estimate personalized between network connectivity
        for (net2 in (net1+1):17) {
                mask_pos <- as.matrix(network_membership$engaged)[, net2] > 0 #length 20484
                FC_net2_pos <- cor(t(as.matrix(cii)[mask_pos & complete.cases(as.matrix(cii)),])) #time series engaged locations by all the time points
                FC_pos <- 0
                net2_sa <- c(sa[left_pos_eng], sa[right_pos_eng]) #need to fix this
                net2_eng <- c(left_pos_lvl[left_pos_eng], right_pos_lvl[right_pos_eng]) #need to fix this
                for (i in 1:ncol(FC_mat_pos)) {
                        for (j in (i+1):ncol(FC_mat_pos)) {
                                if (i < ncol(FC_mat_pos)) {
                                        FC_pos <- FC_pos + (net1_sa[i]*net1_eng[i]*net1_sa[j]*net1_eng[j]*FC_net1_pos[i, j])/(this_sa[i] + this_sa[j])
                                }
                        }
                }
                assign(paste0('FC_pers_', networks[net], '_', networks[net2], '_pos'), FC_pos)
        }

        ###### Estimate group within network connectivity


        ###### Estimate group between network connectivity
        for (net2 in (net+1):17) {
                
                assign(paste0('FC_group_', networks[net], '_', networks[net2], '_pos'), FC_pos)
        }

        ###### Estimate intersection within network connectivity


        ###### Estimate intersection between network connectivity
        for (net2 in (net+1):17) {
                
                assign(paste0('FC_int_', networks[net], '_', networks[net2], '_pos'), FC_pos)
        }
}

###### Output the data
print('Output')

df <- data.frame(subid = subid, sesid = sesid,  #gonna need way more columns... too many to write out like this
                 exp_visuala_pos = exp_visuala_pos, exp_visualb_pos = exp_visualb_pos,
                 exp_somatomotora_pos = exp_somatomotora_pos, 
                 exp_somatomotorb_pos = exp_somatomotorb_pos,
                 exp_dorsalattentiona_pos = exp_dorsalattentiona_pos, 
                 exp_dorsalattentionb_pos = exp_dorsalattentionb_pos,
                 exp_saliencea_pos = exp_saliencea_pos,
                 exp_salienceb_pos = exp_salienceb_pos,
                 exp_limbica_pos = exp_limbica_pos, exp_limbicb_pos = exp_limbicb_pos,
                 exp_controla_pos = exp_controla_pos, exp_controlb_pos = exp_controlb_pos,
                 exp_controlc_pos = exp_controlc_pos, exp_defaulta_pos = exp_defaulta_pos,
                 exp_defaultb_pos = exp_defaultb_pos, exp_defaultc_pos = exp_defaultc_pos,
                 exp_temporalparietal_pos = exp_temporalparietal_pos,
                 FC_visuala_pos = FC_visuala_pos, FC_visualb_pos = FC_visualb_pos,
                 FC_somatomotora_pos = FC_somatomotora_pos,
                 FC_somatomotorb_pos = FC_somatomotorb_pos,
                 FC_dorsalattentiona_pos = FC_dorsalattentiona_pos, 
                 FC_dorsalattentionb_pos = FC_dorsalattentionb_pos,
                 FC_saliencea_pos = FC_saliencea_pos,
                 FC_salienceb_pos = FC_salienceb_pos,
                 FC_limbica_pos = FC_limbica_pos, FC_limbicb_pos = FC_limbicb_pos,
                 FC_controla_pos = FC_controla_pos, FC_controlb_pos = FC_controlb_pos,
                 FC_controlc_pos = FC_controlc_pos, FC_defaulta_pos = FC_defaulta_pos,
                 FC_defaultb_pos = FC_defaultb_pos, FC_defaultc_pos = FC_defaultc_pos,
                 FC_temporalparietal_pos = FC_temporalparietal_pos
                 )

write.csv(df, paste0(outdir, 'sub-', subid, '/ses-', sesid, '/sub-', subid, '_ses-', 
                     sesid, '_surf_network_metrics.csv'), row.names = FALSE)
