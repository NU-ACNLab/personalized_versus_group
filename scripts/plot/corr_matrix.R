### This script creates a correlation matrix of the spatial variables
### for the multiple regression models
### 
### Ellyn Butler
### October 27, 2025

library(ggplot2)
library(ggcorrplot) #Not playing nice with Quest
library(ggpubr)

# Load data
indir <- '/Users/flutist4129/Documents/Northwestern/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/'
comp_df <- read.csv(paste0(indir, 'components_2025-10-20.csv'))
clin_df <- read.csv(paste0(indir, 'prior_subjects_2025-09-19.csv'))
net_df <- read.csv(paste0(indir, 'surf_network_metrics_z1_2025-09-29.csv'))

df <- merge(comp_df, clin_df)
df <- merge(df, net_df)

# Select variables to include in the plot
comps <- names(df)[grepl('component', names(df))]
exps <- names(df)[grepl('exp', names(df))]

# Get the correlation s
d <- df[,c(comps, exps)]

corr_d <- round(cor(d), 3)

get_p_matrix <- function(df) {
  n <- ncol(df)
  p_mat <- matrix(NA, n, n)
  colnames(p_mat) <- rownames(p_mat) <- colnames(df)
  
  for (i in 1:n) {
    for (j in 1:n) {
      p_mat[i, j] <- cor.test(df[[i]], df[[j]])$p.value
    }
  }
  return(p_mat)
}

p_matrix <- get_p_matrix(d)

# Plot
ggcorr_plot <- ggcorrplot(corr_d, p.mat = p_matrix, lab = TRUE, sig.level = 0.05, insig = 'blank') 

png('/Users/flutist4129/Documents/Northwestern/projects/personalized_versus_group/plots/corrplot.png', width=18000, height=17000, res=1000)
ggcorr_plot
dev.off()