### Merge all data types for final sample
###
### Ellyn Butler
### August 13, 2025 - 

# Load libraries
library(tidyverse)

# Load data
clin_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/clinical/clinical_2025-08-11.csv')
demo_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/demographic/demographic_2025-08-11.csv')
net_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/surf_network_metrics_z2_2025-08-13.csv')

# Merge
df <- list(clin_df, demo_df, net_df) %>% 
        reduce(left_join, by = c('subid', 'sesid'))
dim(df) #391

###### Get a sense of missing data
# Columns
df2 <- df[complete.cases(df), ] #278 complete cases

na_counts <- colSums(is.na(df))
na_counts <- sort(na_counts, decreasing = TRUE)

na_counts #FC_int_somatomotora_controlc_pos the highest with 96

# Rows
cols_to_check <- 12:ncol(df)
na_per_row <- rowSums(is.na(df))
has_non_na <- rowSums(!is.na(df[, cols_to_check])) > 0 #has at least some neuroimaging data
eligible_rows <- which(has_non_na)
df[eligible_rows[na_per_row[eligible_rows] == max(na_per_row[eligible_rows])], 'subid'] 
#^ 1000327 has the most missing neuroimaging data
