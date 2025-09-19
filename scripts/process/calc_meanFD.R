### This script calculates mean FD for all chatroom scans
### 
### Ellyn Butler
### August 16, 2025 - September 18, 2025

outdir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/'

basedirs <- c(
  '/projects/b1108/studies/rise/data/processed/neuroimaging/fmriprep_23.2.0',
  '/projects/b1108/studies/crest/data/processed/neuroimaging/fmriprep_23.2.0'
)

# Use Sys.glob to expand the pattern in both directories
files <- Sys.glob(paste0(outdir, 'surf/sub-*/ses-1/func/*quality.csv'))
subids <- sub(".*/sub-([A-Za-z0-9]+)/.*", "\\1", files)

df <- data.frame(
  subid = character(),
  meanFD = numeric(),
  stringsAsFactors = FALSE
)

i = 1
for (subid in subids) {
    files <- unlist(lapply(basedirs, function(b) {
      Sys.glob(file.path(
        b, paste0('sub-', subid), 'ses-1', 'func',
        paste0('sub-', subid, '_ses-1_task-*_run-*_desc-confounds_timeseries.tsv')
      ))
    }))
    fd <- c()
    for (j in 1:length(files)) {
      tmp <- read.table(files[j], sep = '\t', header = TRUE)
      fd <- c(fd, as.numeric(tmp$framewise_displacement))
    }
    meanFD <- mean(fd, na.rm=TRUE)
    df[i, ] <- c(subid, meanFD)
    i = i + 1
}

df$meanFD <- as.numeric(df$meanFD)
df$sesid <- 1
df <- df[, c('subid', 'sesid', 'meanFD')]
write.csv(df, paste0(outdir, 'tabulated/meanFD_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)