### Merge all data types for final sample
###
### Ellyn Butler
### August 17, 2025 - December 13, 2025

# Load libraries
library(tidyverse)

# Load data
clin_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/clinical/clinical_2025-08-11.csv')
demo_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/demographic/demographic_2025-08-11.csv')
net_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/surf_network_metrics_z1_2025-12-13.csv') 
prior_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/prior_subjects_2025-09-19.csv') 
# ^MUST INCLUDE THIS! The surf network metrics file includes subjects from a previous round of processing who must be excluded

# Merge
df <- merge(clin_df, demo_df)
df <- merge(df, net_df) 
df <- merge(df, prior_df)
dim(df) # N = 324

# Brain missing clinical?
net_df$subid[!(net_df$subid %in% clin_df$subid)] # 52 subjects with brain data do not have clinical data

# Brain missing demographic?
net_df$subid[!(net_df$subid %in% demo_df$subid)] # 2 subjects with brain data do not have demographic data (part of 52 above)

# Write out poster data
write.csv(df, paste0('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/combined/combined_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)