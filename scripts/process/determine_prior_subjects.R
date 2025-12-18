### This script finds the subjects with at least 
### five minutes of data after nuisance regression
### and spits out a csv with these subject identifiers
### for use in estimating the prior
###
### Ellyn Butler
### July 8, 2025 - December 17, 2025

# Load data
indir <- '~/Documents/Northwestern/projects/personalized_versus_group/data/processed/'

qual_df <- read.csv(paste0(indir, 'neuroimaging/tabulated/quality_2025-09-18.csv'))
qual_df <- qual_df[qual_df$sesid == 1, ]
fd_df <- read.csv(paste0(indir, 'neuroimaging/tabulated/meanFD_2025-09-18.csv')) # N = 402
clin_df <- read.csv(paste0(indir, 'clinical/clinical_2025-08-11.csv')) # N = 381

# Calculate the number of minutes remaining
qual_df$minutes <- ((qual_df$nTRs - qual_df$nRegressors)*2.05)/60

# Merge fd and clin
df <- merge(fd_df, clin_df) # N = 349

# Sum the number of minutes
df$minutes <- NA
for (i in 1:nrow(df)) {
    df[i, 'minutes'] <- sum(qual_df[qual_df$subid == df[i, 'subid'], 'minutes']) 
}

# Filter out subjects with fewer than 5 minutes remaining
sum(df$minutes < 5) # number of these subjects = 0
# range(df$minutes)... 8.575833 29.041667
#df <- df[df$minutes > 5,]

# Filter out subjects with meanFD > .5
df <- df[df$meanFD < .5, ] # N = 324

# Minutes distribution info
summary(df$minutes)

# Write csv
write.csv(df, paste0(indir, 'neuroimaging/tabulated/prior_subjects_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)