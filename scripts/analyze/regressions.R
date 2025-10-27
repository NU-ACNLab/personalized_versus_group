### This script conducts cross-validation to select a 
### penalty parameter that minimizes out of sample error
### selectively penalizing only the covariates (so not
### the FC estimates)
###
### Ellyn Butler
### October 16, 2025 - October 27, 2025


# Objective: Determine if group FC explains any variance in 
# clinical metrics above and beyond spatial features.


set.seed(1000)

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


# Regression
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
                     fcs_rsq = NA, s_rsq = NA, fc_rsq = NA, F_fc = NA,
                     F_p_fc = NA, F_s = NA, F_p_s = NA, F_nested = NA, 
                     F_p_nested = NA
                     )

for (i in 1:nrow(res_df)) {
    clin <- res_df[i, 'Clinical']
    FCs <- groupcols[grepl(res_df[i, 'Network'], groupcols)]
    
    # FC + spatial model
    fcs_mod <- lm(as.formula(paste0(clin, ' ~ ', paste(c(FCs, comps), collapse = ' + '))), df)
    res_df[i, 'fcs_rsq'] <- summary(fcs_mod)$r.squared  

    # Spatial model
    s_mod <- lm(as.formula(paste0(clin, ' ~ ', paste(comps, collapse = ' + '))), df)
    res_df[i, 's_rsq'] <- summary(s_mod)$r.squared  
    res_df[i, 'F_s'] <- summary(s_mod)$fstatistic[[1]]
    df1 <- summary(s_mod)$fstatistic[2]
    df2 <- summary(s_mod)$fstatistic[3]
    res_df[i, 'F_p_s'] <- pf(res_df[i, 'F_s'], df1, df2, lower.tail = FALSE)

    # FC model
    fc_mod <- lm(as.formula(paste0(clin, ' ~ ', paste(FCs, collapse = ' + '))), df)
    res_df[i, 'fc_rsq'] <- summary(fc_mod)$r.squared  
    res_df[i, 'F_fc'] <- summary(fc_mod)$fstatistic[[1]]
    df1 <- summary(fc_mod)$fstatistic[2]
    df2 <- summary(fc_mod)$fstatistic[3]
    res_df[i, 'F_p_fc'] <- pf(res_df[i, 'F_fc'], df1, df2, lower.tail = FALSE)

    # F test
    anova_res <- anova(s_mod, fcs_mod) 
    res_df[i, 'F_nested'] <- anova_res[['F']][2]
    res_df[i, 'F_p_nested'] <- anova_res[['Pr(>F)']][2]
    #Answers if group FC explains additional variance above and beyond spatial features

    # How much of this variance is unique to FC?
    #print(paste(netnames[j], clin))
    #print((fcs_rsq - s_rsq)/fc_rsq) #Q: Shouldn't this be max 1 by definition?
}
# Spatial... there are only 4 of these models
res_df$F_p_s_fdr <- p.adjust(res_df$F_p_s, method = 'fdr')

res_df[res_df$F_p_s_fdr < 0.05,]
res_df[res_df$F_p_s < 0.01,] #spatial variables can only significantly predict reward sensitivity
res_df[res_df$F_p_s < 0.05,]

# FC... there are 17*4 of these models
res_df$F_p_fc_fdr <- p.adjust(res_df$F_p_fc, method = 'fdr')

res_df[res_df$F_p_fc_fdr < 0.05,] #none
res_df[res_df$F_p_fc < 0.01,] #4

# Nested... there are 17*4 of these models
res_df$F_p_nest_fdr <- p.adjust(res_df$F_p_nested, method = 'fdr')

res_df[res_df$F_p_nested_fdr < 0.05,] #none
res_df[res_df$F_p_nested < 0.01,] #4
