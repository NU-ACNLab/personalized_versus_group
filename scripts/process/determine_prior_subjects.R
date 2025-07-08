### This script finds the subjects with at least 
### five minutes of data after nuisance regression
### and spits out a csv with these subject identifiers
### for use in estimating the prior
###
### Ellyn Butler
### July 8, 2025

indir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/'

df <- read.csv(paste0(indir, '/tabulated/quality_2025-07-08.csv'))

# Select for Chatroom and session 1
df <- df[df$sesid == 1 & df$taskrun == 'task-chatroom_run-01', ]
# Now: N = 390

# Calculate the number of minutes remaining
df$minutes <- ((df$nTRs - df$nRegressors)*2.05)/60

# Number of subjects with fewer than 5 minutes remaining? 3
sum(df$minutes < 5)

# Filter out these subjects
df <- df[df$minutes > 5,]
# Now: N = 387

# Filter for only the subid and sesid columns
df <- df[, c('subid', 'sesid')]

# Write csv
write.csv(df, paste0(indir, 'tabulated/prior_subjects.csv'), row.names = FALSE)