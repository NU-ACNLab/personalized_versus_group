### This script conducts multiple regressions to test
### if components derived from SVD of group FC values
### explain additional variance above and beyond
### spatial components (2% threshold)
###
### Ellyn Butler
### October 16, 2025 - November 3, 2025


# Objective: Determine if group FC explains any variance in 
# clinical metrics above and beyond spatial features.


# Load data
indir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/'
comp_df <- read.csv(paste0(indir, 'spatial_components_2perc_2025-11-03.csv'))
clin_df <- read.csv(paste0(indir, 'prior_subjects_2025-09-19.csv'))
net_df <- read.csv(paste0(indir, 'FC_components_2perc_2025-11-03.csv'))

df <- merge(comp_df, clin_df)
df <- merge(df, net_df)

# Select variables to include in model
FCs <- names(df)[grepl('FC', names(df))]
comps <- names(df)[grepl('component', names(df))]
comps <- comps[!(comps %in% FCs)]

# Regression
clins <- c('rrs_sum', 'bdi_sum', 'punishment', 'reward')

res_df <- data.frame(Clinical = clins,
                     fcs_rsq = NA, s_rsq = NA, fc_rsq = NA, 
                     fcs_rsq_adj = NA, s_rsq_adj = NA, fc_rsq_adj = NA, 
                     F_fc = NA, F_p_fc = NA, F_s = NA, F_p_s = NA, 
                     F_nested = NA, F_p_nested = NA
                     )
for (i in 1:nrow(res_df)) {
    clin <- res_df[i, 'Clinical']
    
    # FC + spatial model
    fcs_mod <- lm(as.formula(paste0(clin, ' ~ ', paste(c(FCs, comps), collapse = ' + '))), df)
    res_df[i, 'fcs_rsq'] <- summary(fcs_mod)$r.squared  
    res_df[i, 'fcs_rsq_adj'] <- summary(fcs_mod)$adj.r.squared  

    # Spatial model
    s_mod <- lm(as.formula(paste0(clin, ' ~ ', paste(comps, collapse = ' + '))), df)
    res_df[i, 's_rsq'] <- summary(s_mod)$r.squared  
    res_df[i, 's_rsq_adj'] <- summary(s_mod)$adj.r.squared  
    res_df[i, 'F_s'] <- summary(s_mod)$fstatistic[[1]]
    df1 <- summary(s_mod)$fstatistic[2]
    df2 <- summary(s_mod)$fstatistic[3]
    res_df[i, 'F_p_s'] <- pf(res_df[i, 'F_s'], df1, df2, lower.tail = FALSE)

    # FC model
    fc_mod <- lm(as.formula(paste0(clin, ' ~ ', paste(FCs, collapse = ' + '))), df)
    res_df[i, 'fc_rsq'] <- summary(fc_mod)$r.squared  
    res_df[i, 'fc_rsq_adj'] <- summary(fc_mod)$adj.r.squared  
    res_df[i, 'F_fc'] <- summary(fc_mod)$fstatistic[[1]]
    df1 <- summary(fc_mod)$fstatistic[2]
    df2 <- summary(fc_mod)$fstatistic[3]
    res_df[i, 'F_p_fc'] <- pf(res_df[i, 'F_fc'], df1, df2, lower.tail = FALSE)

    # F test
    anova_res <- anova(s_mod, fcs_mod) 
    res_df[i, 'F_nested'] <- anova_res[['F']][2]
    res_df[i, 'F_p_nested'] <- anova_res[['Pr(>F)']][2]
    #Answers if group FC explains additional variance above and beyond spatial features
}

# Spatial
res_df[res_df$F_p_s < 0.05,] #reward
res_df[res_df$F_p_s < 0.05/4,] #reward

# FC
res_df[res_df$F_p_fc < 0.05,] #none
res_df[res_df$F_p_fc < 0.05/4,] #none

# Nested
res_df[res_df$F_p_nested < 0.05,] #none
res_df[res_df$F_p_nested < 0.05/4,] #none
