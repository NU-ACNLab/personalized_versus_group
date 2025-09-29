### Merge all data types for final sample
###
### Ellyn Butler
### August 17, 2025 - September 29, 2025

# Load libraries
library(tidyverse)

# Load data
clin_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/clinical/clinical_2025-08-11.csv')
demo_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/demographic/demographic_2025-08-11.csv')
net_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/surf_network_metrics_z1_2025-08-17.csv') # complete cases 386 here

# Merge
df <- merge(clin_df, demo_df)
df <- merge(df, net_df) 
dim(df) # N = 335

# Brain missing clinical?
net_df$subid[!(net_df$subid %in% clin_df$subid)] # 52 subjects with brain data do not have clinical data

# Brain missing demographic?
net_df$subid[!(net_df$subid %in% demo_df$subid)] # 2 subjects with brain data do not have demographic data (part of 52 above)

# Get a sense of missing data
df2 <- df[complete.cases(df[, c(1:8, 12:ncol(df))]), ] #don't care if no gender, race, latino
dim(df2) # N = 334

# Write out poster data
write.csv(df2, paste0('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/combined/poster_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)