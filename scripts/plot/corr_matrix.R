### This script creates a correlation matrix of the spatial variables
### for the multiple regression models
### 
### Ellyn Butler
### October 27, 2025 - October 30, 2025

library(ggplot2)
library(ggcorrplot) #Not playing nice with Quest
library(ggpubr)
library(dplyr)

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
names(d) <- recode(names(d), 'VisCent_component1' = 'Visual A C1', 
                             'VisCent_component2' = 'Visual A C2',
                             'VisCent_component3' = 'Visual A C3',
                             'VisCent_component4' = 'Visual A C4',
                             'VisCeri_component1' = 'Visual B C1',
                             'VisCeri_component2' = 'Visual B C2',
                             'VisCeri_component3' = 'Visual B C3',
                             'SomMotA_component1' = 'Somatomotor A C1',
                             'SomMotA_component2' = 'Somatomotor A C2',
                             'SomMotA_component3' = 'Somatomotor A C3',
                             'SomMotA_component4' = 'Somatomotor A C4',
                             'SomMotB_component1' = 'Somatomotor B C1',
                             'SomMotB_component2' = 'Somatomotor B C2',
                             'DorsAttnB_component1' = 'Dorsal Attention B C1',
                             'DorsAttnB_component2' = 'Dorsal Attention B C2',
                             'DorsAttnB_component3' = 'Dorsal Attention B C3',
                             'SalVenAttnB_component1' = 'Salience/Ventral Attention B C1',
                             'SalVenAttnB_component2' = 'Salience/Ventral Attention B C2',
                             'LimbicB_component1' = 'Limbic B C1',
                             'LimbicB_component2' = 'Limbic B C2',
                             'LimbicB_component3' = 'Limbic B C3',
                             'ContB_component1' = 'Control B C1',
                             'ContB_component2' = 'Control B C2',
                             'ContC_component1' = 'Control C C1',
                             'ContC_component2' = 'Control C C2',
                             'ContC_component3' = 'Control C C3',
                             'TempPar_component1' = 'Temporal Parietal C1',
                             'TempPar_component2' = 'Temporal Parietal C2',
                             'exp_visuala_pos' = 'Visual A Expansion',
                             'exp_visualb_pos' = 'Visual B Expansion',
                             'exp_somatomotora_pos' = 'Somatomotor A Expansion',
                             'exp_somatomotorb_pos' = 'Somatomotor B Expansion',
                             'exp_dorsalattentiona_pos' = 'Dorsal Attention A Expansion',
                             'exp_dorsalattentionb_pos' = 'Dorsal Attention B Expansion',
                             'exp_saliencea_pos' = 'Salience A Expansion',
                             'exp_salienceb_pos' = 'Salience B Expansion',
                             'exp_limbica_pos' = 'Limbic A Expansion',
                             'exp_limbicb_pos' = 'Limbic B Expansion',
                             'exp_controla_pos' = 'Control A Expansion',
                             'exp_controlb_pos' = 'Control B Expansion',
                             'exp_controlc_pos' = 'Control C Expansion',
                             'exp_defaulta_pos' = 'Default A Expansion',
                             'exp_defaultb_pos' = 'Default B Expansion',
                             'exp_defaultc_pos' = 'Default C Expansion',
                             'exp_temporalparietal_pos' = 'Temporal Parietal Expansion'
                             )

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