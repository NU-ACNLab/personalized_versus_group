### This script cleans SES data files
###
### Ellyn Butler
### November 2, 2025

# Load libraries
library(tidyverse)
library(dplyr)

# Load data
rise_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/demographic/ProjectRISE-P1ChildhoodSESInterv_DATA_2025-11-02_1511.csv')
crest_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/demographic/ProjectCREST-P1ChildhoodSESInterv_DATA_2025-11-02_1513.csv')

rise_df$subid <- rise_df$rise_id
crest_df$subid <- crest_df$crest_id

# Get rid of nonoverlapping columns
rise_df$rise_id <- NULL
rise_df$ses_pr_fam_struc_age_16_other <- NULL
rise_df$ses_pr_fam_struc_age_16 <- NULL
crest_df$crest_id <- NULL
crest_df$ses_pr_fam_struc_age_16_other <- NULL
crest_df$ses_pr_fam_struc_age_16 <- NULL

if (all(names(rise_df) == names(crest_df)) == TRUE) {
    df <- rbind(rise_df, crest_df)
}

df$sesid <- 1

# Filter data 
# ses_pr_parent2: Is there another parent/guardian that spends/spent
# Average guardian years of education (ses_pr_edu_years + ses_pr_parent2_edu_years)/2
# ses_pr_income_category: Family's total gross income in category

df$ses_pr_edu_years[is.na(df$ses_pr_edu_years)] <- df$ses_pr_parent2_edu_years[is.na(df$ses_pr_edu_years)]
df$ses_pr_parent2_edu_years[is.na(df$ses_pr_parent2_edu_years)] <- df$ses_pr_edu_years[is.na(df$ses_pr_parent2_edu_years)]

df$average_edu_years <- rowSums(df[, c('ses_pr_edu_years', 'ses_pr_parent2_edu_years')])/2
df$income_category <- recode(df$ses_pr_income_category, `1`='$0 - $4,999', 
                             `2`='$5,000 - $19,999', `3`='$20,000 - $34,999', 
                             `4`='$35,000 - $49,999', `5`='$50,000 - $74,999', 
                             `6`='$75,000 - $99,999', `7`='$100,000 - $149,999', 
                             `8`='$150,000 - $199,999', `9`='$200,000 - $249,999',
                             `10`='$250,000 - $299,999', `11`='$300,000 and higher', 
                             `12`='Not known')
df$income_category <- ordered(df$income_category, c('$0 - $4,999', '$5,000 - $19,999', 
                        '$20,000 - $34,999', '$35,000 - $49,999', '$50,000 - $74,999',
                        '$75,000 - $99,999', '$100,000 - $149,999', '$150,000 - $199,999',
                        '$200,000 - $249,999', '$250,000 - $299,999', '$300,000 and higher',
                        'Not known'))

df <- df[, c('subid', 'sesid', 'average_edu_years', 'income_category')]

# Write
write.csv(df, paste0('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/demographic/ses_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)