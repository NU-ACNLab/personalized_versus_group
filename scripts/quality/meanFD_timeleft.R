### This script plots mean FD and time left after
### nuisance regression against the number of networks
### missing with z = 3
###
### Ellyn Butler
### August 16, 2025

# Load libraries 
library(ggplot2)

# Load data
net_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/surf_network_metrics_2025-08-07.csv')
time_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/quality_2025-07-08.csv')
fd_df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/meanFD.csv')

# Calculate time left after nuisance regression in seconds
time_df$time_left <- ((time_df$nTRs - time_df$nRegressors)*2.05)/60

# Calculate number of network metrics missing
net_df$num_net_missing <- rowSums(is.na(net_df[, 3:ncol(net_df)]))

# Subset to only first session
time_df <- time_df[which(time_df$sesid == 1 & time_df$taskrun == 'task-chatroom_run-01'), ]

# Merge
df <- merge(net_df, time_df)
df <- merge(df, fd_df)

# Correlations
cor(df$meanFD, df$num_net_missing) #-0.100
cor(df$time_left, df$num_net_missing) #-0.102

# Histograms (to determine subsetting)
meanFD_hist <- ggplot(df, aes(x = meanFD)) + geom_histogram() + 
                theme_linedraw() + xlab('Mean FD')
time_left_hist <- ggplot(df, aes(x = time_left)) + geom_histogram() + 
                theme_linedraw() + xlab('Time Left (minutes)')

# Subset
df <- df[which(df$meanFD < .5), ]

# Scatterplots
meanFD_scat <- ggplot(df, aes(x = num_net_missing, y = meanFD)) + geom_point() + 
                theme_linedraw() + xlab('# Network Metrics Missing') + ylab('Mean FD')
time_left_scat <- ggplot(df, aes(x = num_net_missing, y = time_left)) + geom_point() + 
                theme_linedraw() + xlab('# Network Metrics Missing') + ylab('Time Left (minutes)')

# Correlations
cor(df$meanFD, df$num_net_missing) #-0.054
cor(df$time_left, df$num_net_missing) #-0.115

# Conclusion:
# Maybe subset on meanFD just because some of the values are pretty insane 
# But wait to do this for the paper, as this will change the prior