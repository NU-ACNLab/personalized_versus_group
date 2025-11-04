### This script details what exactly is missing with 
### respect to neuroimaging data
###
### Ellyn Butler
### October 30, 2025

# Load data
indir <- '~/Documents/Northwestern/projects/personalized_versus_group/data/processed/'

screen_df <- read.csv(paste0(indir'neuroimaging/tabulated/started_screen.csv'))
t1_mri_df <- read.csv(paste0('neuroimaging/tabulated/rise_crest_t1_neuro_2025-06-18.csv'))
qual_df <- read.csv(paste0(indir, 'neuroimaging/tabulated/quality_2025-09-18.csv'))
qual_df <- qual_df[qual_df$sesid == 1, ]
fd_df <- read.csv(paste0(indir, 'neuroimaging/tabulated/meanFD_2025-09-18.csv'))

### Compare screening group and T1 MRI group
# Sanity check: everyone who completed T1 MRI also screened?
t1s_df <- merge(screen_df, t1_mri_df)
nrow(t1_mri_df) - nrow(t1s_df) #should be zero - but it is 4

# Number of those screened who do not have MRI
nrow(screen_df) - nrow(t1_mri_df) - (nrow(t1_mri_df) - nrow(t1s_df))

# Number of subjects with fewer than 5 minutes of data left after confound regression
qual_df$minutes <- ((qual_df$nTRs - qual_df$nRegressors)*2.05)/60
fd_df$minutes <- NA
for (i in 1:nrow(fd_df)) {
    fd_df[i, 'minutes'] <- sum(qual_df[qual_df$subid == fd_df[i, 'subid'], 'minutes']) 
}
sum(fd_df$minutes < 5) 

# Number of subjects with a mean framewise displacement greater than 0.5mm
nrow(fd_df[fd_df$meanFD > .5,])
