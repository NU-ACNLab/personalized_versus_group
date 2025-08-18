### This script compares three different comprises for 
### the final sample in terms of z and number of networks
### missing, and how this effects the final N
###
### Ellyn Butler
### August 17, 2025

# Load data
basedir <- '~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/'
z1_df <- read.csv(paste0(basedir, 'surf_network_metrics_z1_2025-08-17.csv'))
z2_df <- read.csv(paste0(basedir, 'surf_network_metrics_z2_2025-08-13.csv'))
z3_df <- read.csv(paste0(basedir, 'surf_network_metrics_2025-08-07.csv'))

##### 1) No missing networks and z = 1
z1_df2 <- z1_df[complete.cases(z1_df[, 3:ncol(z1_df)]), ]
dim(z1_df2) # N = 386... only lose 1 person

##### 2) 2 missing networks and z = 2
na_counts <- colSums(is.na(z2_df))
na_counts <- sort(na_counts, decreasing = TRUE)

z2_df2 <- z2_df[, !(names(z2_df) %in% names(z2_df)[grepl('somatomotora', names(z2_df))])] #Getting rid of somatomotora
na_counts <- colSums(is.na(z2_df2))
na_counts <- sort(na_counts, decreasing = TRUE)

z2_df3 <- z2_df2[, !(names(z2_df2) %in% names(z2_df2)[grepl('controlc', names(z2_df2))])] #Getting rid of controlc
na_counts <- colSums(is.na(z2_df3))
na_counts <- sort(na_counts, decreasing = TRUE) #next to get rid of would be salienceb

z2_df4 <- z2_df3[complete.cases(z2_df3[, 3:ncol(z2_df3)]), ]
dim(z2_df4) # N = 363... only lose 24 people

##### 3) 3 missing networks and z = 3... this isn't tenable
# Chose 3 networks because it became less clear which one to eliminate next at this point
na_counts <- colSums(is.na(z3_df))
na_counts <- sort(na_counts, decreasing = TRUE)

z3_df2 <- z3_df[, !(names(z3_df) %in% names(z3_df)[grepl('somatomotora', names(z3_df))])] #Getting rid of somatomotora
na_counts <- colSums(is.na(z3_df2))
na_counts <- sort(na_counts, decreasing = TRUE)

z3_df3 <- z3_df2[, !(names(z3_df2) %in% names(z3_df2)[grepl('salienceb', names(z3_df2))])] #Getting rid of salienceb... eek
na_counts <- colSums(is.na(z3_df3))
na_counts <- sort(na_counts, decreasing = TRUE) 

z3_df4 <- z3_df3[, !(names(z3_df3) %in% names(z3_df3)[grepl('controlc', names(z3_df3))])] #Getting rid of controlc
na_counts <- colSums(is.na(z3_df4))
na_counts <- sort(na_counts, decreasing = TRUE) 

z3_df5 <- z3_df4[complete.cases(z3_df4[, 3:ncol(z3_df4)]), ]
dim(z3_df5) # N = 178... loses 209 people