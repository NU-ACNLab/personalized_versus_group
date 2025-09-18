### This script calculates mean FD for all chatroom scans
### 
### Ellyn Butler
### August 16, 2025

basedirs <- c(
  '/projects/b1108/studies/rise/data/processed/neuroimaging/fmriprep_23.2.0',
  '/projects/b1108/studies/crest/data/processed/neuroimaging/fmriprep_23.2.0'
)

# Use Sys.glob to expand the pattern in both directories
files <- unlist(lapply(basedirs, function(b) {
  Sys.glob(file.path(
    b,
    'sub-*', 'ses-1', 'func',
    'sub-*_ses-1_task-chatroom_run-*_desc-confounds_timeseries.tsv'
  ))
}))

df <- data.frame(
  subid = character(),
  meanFD = numeric(),
  stringsAsFactors = FALSE
)

i = 1
for (file in files) {
    tmp <- read.table(file, sep = '\t', header = TRUE)
    subid <- sub(".*sub-([0-9]+).*", "\\1", file)
    tmp$framewise_displacement <- as.numeric(tmp$framewise_displacement)
    meanFD <- mean(tmp$framewise_displacement, na.rm=TRUE)
    df[i, ] <- c(subid, meanFD)
    i = i + 1
}

df$meanFD <- as.numeric(df$meanFD)
df$sesid <- 1
df <- df[, c('subid', 'sesid', 'meanFD')]
write.csv(df, '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/meanFD.csv', row.names = FALSE)