### This script conducts cross-validation to select a 
### penalty parameter that minimizes out of sample error
### selectively penalizing only the covariates (so not
### the FC estimates)
###
### Ellyn Butler
### October 16, 2025

library(glmnet)

# Load data
indir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/'
comp_df <- read.csv(paste0(indir, 'components_2025-10-16.csv'))
clin_df <- read.csv(paste0(indir, 'prior_subjects_2025-09-19.csv'))
net_df <- read.csv(paste0(indir, 'surf_network_metrics_z1_2025-09-29.csv'))

df <- merge(comp_df, clin_df)
df <- merge(df, net_df)

# Select variables to include in model
groupcols <- names(df)[grepl('group', names(df))]

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

lambda_seq <- 10^seq(2, -2, by = -.1)
for (j in 1:17) {
    for (clin in clins) {
        FCs <- groupcols[grepl(netnames2[j], groupcols)]
        exps <- names(df)[grepl('exp', names(df))]
        comps <- names(df)[grepl('component', names(df))]
        X <- as.matrix(df[, c(FCs, exps, comps)])
        y <- as.matrix(df[, clin])
        penalized_columns <- c(rep(0, length(FCs)), rep(1, length(exps)), rep(1, length(comps)))

        # Full model
        cv_ridge <- cv.glmnet(X, y, alpha = 0, lambda = lambda_seq,
                    penalty.factor = penalized_columns)
        best_lambda <- cv_ridge$lambda.min

        ridge_model <- cv_ridge$glmnet.fit
        coef(ridge_model, s = best_lambda)
        head(ridge_model)
        full_rsq <- 

        # FC model
        fc_mod <- lm(as.formula(paste0(clin, ' ~ ', paste(FCs, collapse = '+'))), df)
        reduced_rsq <- summary(fc_mod)$r.squared  

        # F test
        
    }
}