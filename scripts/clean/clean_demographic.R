### This script cleans demographic data files
###
### Ellyn Butler
### August 11, 2025

# Load libraries
library(haven)
library(tidyverse)

# Load data
rise_demo_df <- read_sav('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/demographic/RISE_Demo_8.7.2025.sav')
rise_bdi_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/clinical/ProjectRISE-17BDI_DATA_2025-08-07_1428.csv')
crest_demo_df <- read_sav('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/demographic/CREST_Demo_8.7.2025.sav')
crest_bdi_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/clinical/ProjectCREST-15BDI_DATA_2025-08-07_1440.csv')

# Filter BDI data to get the date
rise_bdi_df$sesid <- recode(rise_bdi_df$redcap_event_name, 't1s2_b1_arm_1' = 1,
                            't2_b1_arm_1' = 2, 't3s2_b1_arm_1' = 3,
                            't4_b1_arm_1' = 4, 't5s2_b1_arm_1' = 5)
rise_bdi_df$redcap_event_name <- NULL
rise_bdi_df <- rise_bdi_df[which(rise_bdi_df$sesid == 1), ]

crest_bdi_df$sesid <- recode(crest_bdi_df$redcap_event_name, 't1_b2_arm_1' = 1,
                            't2_b1_arm_1' = 2, 't3_b2_arm_1' = 3,
                            't4_b1_arm_1' = 4, 't5_b2_arm_1' = 5,
                            't6_b1_arm_1' = 6)
crest_bdi_df$redcap_event_name <- NULL
crest_bdi_df <- crest_bdi_df[which(crest_bdi_df$sesid == 1), ]

# Merge data
rise_df <- merge(rise_demo_df, rise_bdi_df, by = 'rise_id')
rise_df$subid <- rise_df$rise_id
rise_df$rise_id <- NULL
rise_df$bdi_risk_calc <- NULL
rise_df$rise_group <- NULL
crest_df <- merge(crest_demo_df, crest_bdi_df, by = 'crest_id')
crest_df$subid <- crest_df$crest_id
crest_df$crest_id <- NULL
crest_df$Correction_Notes <- NULL
crest_df$bdi_risk <- NULL
crest_df$crest_group <- NULL

if (all(names(rise_df) == names(crest_df)) == TRUE) {
    df <- rbind(rise_df, crest_df)
}

# Calculate age
df$demo_dob <- as.Date(df$demo_dob, '%Y-%m-%d')
df$bdi_date_time <- as.Date(df$bdi_date_time, '%Y-%m-%d')
df$age <- (df$bdi_date_time - df$demo_dob)/365
df$age <- as.numeric(df$age)

# Clean up
df$sex <- df$demo_sex
df$gender <- df$demo_gender
df$race <- df$demo_race
df$latino <- df$demo_latino

# Filter data (subid, sesid, age, sex, gender, race, latino)
df <- df[, c('subid', 'sesid', 'age', 'sex', 'gender', 'race', 'latino')]

# Write
write.csv(df, paste0('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/demographic/demographic_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)