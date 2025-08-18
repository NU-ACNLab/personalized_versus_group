### Merge all data types for final sample
###
### Ellyn Butler
### August 17, 2025 

# Load libraries
library(tidyverse)

# Load data
clin_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/clinical/clinical_2025-08-11.csv')
demo_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/demographic/demographic_2025-08-11.csv')
net_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/surf_network_metrics_z1_2025-08-17.csv')

# Merge
df <- list(clin_df, demo_df, net_df) %>% 
        reduce(left_join, by = c('subid', 'sesid'))
dim(df) # N = 391

# Get a sense of missing data
df2 <- df[complete.cases(df), ] 
dim(df2) # N = 329

# Write out poster data
write.csv(paste0('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/combined/poster_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)