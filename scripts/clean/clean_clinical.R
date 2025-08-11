### This script cleans the raw clinical data to 
### make it usable for analyses
###
### Ellyn Butler
### August 7, 2025 - 

# Load libraries
library(tidyverse)
library(psych)
library(ggplot2)
library(ggcorrplot)

####### RISE

# Load data
rise_bdi_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/clinical/ProjectRISE-17BDI_DATA_2025-08-07_1428.csv')
rise_rrs_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/clinical/ProjectRISE-22RRS_DATA_2025-08-07_1428.csv')
rise_spsrq_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/clinical/ProjectRISE-2SPSRQ_DATA_2025-08-07_1425.csv')
rise_psqi_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/clinical/ProjectRISE-7PSQI_DATA_2025-08-07_1429.csv')

# Recode session
rise_bdi_df$sesid <- recode(rise_bdi_df$redcap_event_name, 't1s2_b1_arm_1' = 1,
                            't2_b1_arm_1' = 2, 't3s2_b1_arm_1' = 3,
                            't4_b1_arm_1' = 4, 't5s2_b1_arm_1' = 5)
rise_bdi_df$redcap_event_name <- NULL
rise_rrs_df$sesid <- recode(rise_rrs_df$redcap_event_name, 't1s2_b1_arm_1' = 1,
                            't3s2_b1_arm_1' = 3, 't5s2_b1_arm_1' = 5)
rise_rrs_df$redcap_event_name <- NULL
rise_spsrq_df$sesid <- recode(rise_spsrq_df$redcap_event_name, 't1s1_b1_arm_1' = 1,
                               't3s1_b1_arm_1' = 3, 't5s1_b1_arm_1' = 5)
rise_spsrq_df$redcap_event_name <- NULL
rise_psqi_df$sesid <- recode(rise_psqi_df$redcap_event_name, 't1s1_b1_arm_1' = 1,
                            't2_b1_arm_1' = 2, 't3s1_b1_arm_1' = 3,
                            't4_b1_arm_1' = 4, 't5s1_b1_arm_1' = 5)
rise_psqi_df$redcap_event_name <- NULL                       

# Merge and filter
rise_df <- list(rise_bdi_df, rise_rrs_df, rise_spsrq_df, rise_psqi_df) %>%
                reduce(left_join, by = c('rise_id', 'sesid'))
rise_df <- rise_df[which(rise_df$sesid == 1), ]
rise_df$subid <- rise_df$rise_id
rise_df$rise_id <- NULL
rise_df <- rise_df %>% relocate('subid', 'sesid')
rise_df$bdi_risk_calc <- NULL

####### CREST

# Load data
crest_bdi_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/clinical/ProjectCREST-15BDI_DATA_2025-08-07_1440.csv')
crest_rrs_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/clinical/ProjectCREST-7RRS_DATA_2025-08-07_1440.csv')
crest_spsrq_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/clinical/ProjectCREST-2SPSRQ_DATA_2025-08-07_1439.csv')
crest_psqi_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/clinical/ProjectCREST-5PSQI_DATA_2025-08-07_1439.csv')

# Recode session
crest_bdi_df$sesid <- recode(crest_bdi_df$redcap_event_name, 't1_b2_arm_1' = 1,
                            't2_b1_arm_1' = 2, 't3_b2_arm_1' = 3,
                            't4_b1_arm_1' = 4, 't5_b2_arm_1' = 5,
                            't6_b1_arm_1' = 6)
crest_bdi_df$redcap_event_name <- NULL
crest_rrs_df$sesid <- recode(crest_rrs_df$redcap_event_name, 't1_b1_arm_1' = 1,
                            't3_b1_arm_1' = 3, 't5_b1_arm_1' = 5)
crest_rrs_df$redcap_event_name <- NULL
crest_spsrq_df$sesid <- recode(crest_spsrq_df$redcap_event_name, 't1_b1_arm_1' = 1,
                            't3_b1_arm_1' = 3, 't5_b1_arm_1' = 5)
crest_spsrq_df$redcap_event_name <- NULL
crest_psqi_df$sesid <- recode(crest_psqi_df$redcap_event_name, 't1_b1_arm_1' = 1,
                            't3_b1_arm_1' = 3, 't5_b1_arm_1' = 5)
crest_psqi_df$redcap_event_name <- NULL                       

# Merge and filter
crest_df <- list(crest_bdi_df, crest_rrs_df, crest_spsrq_df, crest_psqi_df) %>%
                reduce(left_join, by = c('crest_id', 'sesid'))
crest_df <- crest_df[which(crest_df$sesid == 1), ]
crest_df$subid <- crest_df$crest_id
crest_df$crest_id <- NULL
crest_df <- crest_df %>% relocate('subid', 'sesid')
crest_df$bdi_risk <- NULL

####### Combine across RISE and CREST

if (all(names(rise_df) == names(crest_df)) == TRUE) {
    df <- rbind(rise_df, crest_df)
}

# Understand
dim(df) # there are 558 subjects with ses 1 data
dim(df[complete.cases(df[, c('bdi_1', 'rrs_1', 'spsrq_1', 'psqi_1_hh')]), ] )
# ^ 434 of these started all of the surveys

# How much of the BDI, RRS, and SPSRQ do these people have?
df <- df[complete.cases(df[, c('subid', 'sesid', paste0('bdi_', 1:21), 
            paste0('rrs_', 1:10), paste0('spsrq_', 1:48))]), ]
dim(df) #391... this is the way to go

####### Calculate summary scores

### RRS
rrs_fa1 <- fa(df[, paste0('rrs_', 1:10)], n.factors = 1)
rrs_fa2 <- fa(df[, paste0('rrs_', 1:10)], n.factors = 2)
anova(rrs_fa1, rrs_fa2) # > Implies one factor solution... but weird identical?

# Factor structure
cormat <- round(cor(df[, paste0('rrs_', 1:10)]), 2)
corplot <- ggcorrplot(cormat, lab = TRUE, lab_size = 2) + ggtitle('RRS') #1 factor looks reasonable

eigenvalues1 <- eigen(cormat)$values
eigen_df1 <- data.frame(matrix(NA, nrow=length(eigenvalues1), ncol=2))
names(eigen_df1) <- c('compnum', 'eigen')
eigen_df1$compnum <- 1:10
eigen_df1$eigen <- eigenvalues1

rrs_scree <- ggplot(eigen_df1, aes(x=compnum, y=eigen)) +
    geom_line(stat = 'identity') + geom_point() +  theme_minimal() +
    xlab('Component Number') + ylab('Eigenvalues of Components') +
    scale_y_continuous(limits=c(0, 10)) + ggtitle('RRS') +
    theme(plot.title = element_text(size=12), axis.title = element_text(size=10),
      axis.text = element_text(size=6)) #really 1 factor

# Sum score
df$rrs_sum <- rowSums(df[, paste0('rrs_', 1:10)])

### BDI
bdi_fa1 <- fa(df[, paste0('bdi_', 1:21)], n.factors = 1)
bdi_fa2 <- fa(df[, paste0('bdi_', 1:21)], n.factors = 2)
anova(bdi_fa1, bdi_fa2) # > Implies one factor solution... but weird identical?

cormat <- round(cor(df[, paste0('bdi_', 1:21)]), 2)
corplot <- ggcorrplot(cormat, lab = TRUE, lab_size = 2) + ggtitle('BDI') #1 factor looks reasonable

eigenvalues1 <- eigen(cormat)$values
eigen_df1 <- data.frame(matrix(NA, nrow=length(eigenvalues1), ncol=2))
names(eigen_df1) <- c('compnum', 'eigen')
eigen_df1$compnum <- 1:21
eigen_df1$eigen <- eigenvalues1

bdi_scree <- ggplot(eigen_df1, aes(x=compnum, y=eigen)) +
    geom_line(stat='identity') + geom_point() +  theme_minimal() +
    xlab('Component Number') + ylab('Eigenvalues of Components') +
    scale_y_continuous(limits=c(0, 10)) + ggtitle('BDI') +
    theme(plot.title = element_text(size=12), axis.title = element_text(size=10),
      axis.text = element_text(size=6)) #really 1 factor

# Sum score
df$bdi_sum <- rowSums(df[, paste0('bdi_', 1:21)])

### SPSRQ
spsrq_fa1 <- fa(df[, paste0('spsrq_', 1:48)], n.factors = 1)
spsrq_fa2 <- fa(df[, paste0('spsrq_', 1:48)], n.factors = 2)
anova(spsrq_fa1, spsrq_fa2) # > Implies one factor solution... but weird identical? Yes, because looks like a two factor solution based on scree

cormat <- round(cor(df[, paste0('spsrq_', 1:48)]), 2)
corplot <- ggcorrplot(cormat, lab = TRUE, lab_size = 2) + ggtitle('SPSRQ') 

eigenvalues1 <- eigen(cormat)$values
eigen_df1 <- data.frame(matrix(NA, nrow=length(eigenvalues1), ncol=2))
names(eigen_df1) <- c('compnum', 'eigen')
eigen_df1$compnum <- 1:48
eigen_df1$eigen <- eigenvalues1

spsrq_scree <- ggplot(eigen_df1, aes(x=compnum, y=eigen)) +
    geom_line(stat='identity') + geom_point() +  theme_minimal() +
    xlab('Component Number') + ylab('Eigenvalues of Components') +
    scale_y_continuous(limits=c(0, 10)) + ggtitle('SPSRQ') +
    theme(plot.title = element_text(size=12), axis.title = element_text(size=10),
      axis.text = element_text(size=6)) #really 2 factors

# Sum score
# https://arc.psych.wisc.edu/self-report/sensitivity-to-punishment-and-reward-questionnaire-spsrq/
df$punishment <- rowSums(df[, paste0('spsrq_', seq(1, 47, 2))])
df$reward <- rowSums(df[, paste0('spsrq_', seq(2, 48, 2))])

####### Finalize and export

df <- df[, c('subid', 'sesid', 'rrs_sum', 'bdi_sum', 'punishment', 'reward')]
write.csv(df, paste0('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/clinical/clinical_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)