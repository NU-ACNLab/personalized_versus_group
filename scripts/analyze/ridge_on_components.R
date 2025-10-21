### This script conducts cross-validation to select a 
### penalty parameter that minimizes out of sample error
### selectively penalizing only the covariates (so not
### the FC estimates)
###
### Ellyn Butler
### October 16, 2025 - October 20, 2025


# Objective: Determine if group FC explain any variance in 
# clinical metrics above and beyond spatial features.


set.seed(1000)

library(glmnet)

# Load data
indir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/'
comp_df <- read.csv(paste0(indir, 'components_2025-10-20.csv'))
clin_df <- read.csv(paste0(indir, 'prior_subjects_2025-09-19.csv'))
net_df <- read.csv(paste0(indir, 'surf_network_metrics_z1_2025-09-29.csv'))

df <- merge(comp_df, clin_df)
df <- merge(df, net_df)

# Select variables to include in model
groupcols <- names(df)[grepl('group', names(df))]
comps <- names(df)[grepl('component', names(df))]
exps <- names(df)[grepl('exp', names(df))]

# Ridge regression
netnames <- c('VisCent', 'VisCeri', 'SomMotA', 'SomMotB', 'DorsAttnA',
              'DorsAttnB', 'SalVenAttnA', 'SalVenAttnB', 'LimbicA',
              'LimbicB', 'ContA', 'ContB', 'ContC', 'DefaultA',
              'DefaultB', 'DefaultC', 'TempPar')
netnames2 <- c('visuala', 'visualb', 'somatomotora', 'somatomotorb',
               'dorsalattentiona', 'dorsalattentionb', 'saliencea',
               'salienceb', 'limbica', 'limbicb', 'controla', 'controlb',
               'controlc', 'defaulta', 'defaultb', 'defaultc',
               'temporalparietal')
clins <- c('rrs_sum', 'bdi_sum', 'punishment', 'reward')

res_df <- data.frame(Network = rep(netnames2, each = length(clins)),
                     Clinical = rep(clins, length(netnames)),
                     fcs_rsq = NA, s_rsq = NA, fc_rsq = NA, F = NA, 
                     F_p = NA
                     )
for (i in 1:nrow(res_df)) {
    clin <- res_df[i, 'Clinical']
    FCs <- groupcols[grepl(res_df[i, 'Network'], groupcols)]
    
    # FC + spatial model
    fcs_mod <- lm(as.formula(paste0(clin, ' ~ ', paste(c(FCs, exps, comps), collapse = ' + '))), df)
    res_df[i, 'fcs_rsq'] <- summary(fcs_mod)$r.squared  

    # Spatial model
    s_mod <- lm(as.formula(paste0(clin, ' ~ ', paste(c(exps, comps), collapse = ' + '))), df)
    res_df[i, 's_rsq'] <- summary(s_mod)$r.squared  

    # FC model
    fc_mod <- lm(as.formula(paste0(clin, ' ~ ', paste(FCs, collapse = ' + '))), df)
    res_df[i, 'fc_rsq'] <- summary(fc_mod)$r.squared  

    # F test
    anova_res <- anova(s_mod, fcs_mod) 
    res_df[i, 'F'] <- anova_res[['F']][2]
    res_df[i, 'F_p'] <- anova_res[['Pr(>F)']][2]
    #Answers if group FC explains additional variance above and beyond spatial

    # How much of this variance is unique to FC?
    #print(paste(netnames[j], clin))
    #print((fcs_rsq - s_rsq)/fc_rsq) #Q: Shouldn't this be max 1 by definition?
}

res_df$F_p_fdr <- p.adjust(res_df$F_p, method = 'fdr')

res_df[res_df$F_p_fdr < 0.05,]
res_df[res_df$F_p < 0.01,]