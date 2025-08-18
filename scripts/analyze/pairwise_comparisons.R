### This script conducts the pairwise comparisons and
### generates a plot with these results
###
### Ellyn Butler
### August 17, 2025 - 

# Load libraries
library(ggplot2)
library(psych)

# Load data
df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/combined/poster_2025-08-17.csv')
df <- df[, c('subid', 'sesid', 'rrs_sum', 'bdi_sum', 'punishment', 'reward', 
             names(df)[grepl('^(?:[^_]*_){3}[^_]*$', names(df))])]

# A. Group versus personalized
gp_df <- data.frame(clins = rep(c('rrs_sum', 'bdi_sum', 'punishment', 'reward'), 17),
                    nets = c(rep('visuala', 4), rep('visualb', 4), rep('somatomotora', 4),
                             rep('somatomotorb', 4), rep('dorsalattentiona', 4),
                             rep('dorsalattentionb', 4), rep('saliencea', 4),
                             rep('salienceb', 4), rep('limbica', 4), rep('limbicb', 4),
                             rep('controla', 4), rep('controlb', 4), rep('controlc', 4), 
                             rep('defaulta', 4), rep('defaultb', 4), rep('defaultc', 4), 
                             rep('temporalparietal', 4)),
                    r12 = NA, r13 = NA, r23 = NA, t = NA, p = NA)
for (i in 1:nrow(gp_df)) {
    clin <- gp_df[i, 'clins']
    net <- gp_df[i, 'nets']
    gname <- paste0('FC_group_', net, '_pos')
    pname <- paste0('FC_pers_', net, '_pos')
    r12 <- cor(df[, gname], df[, clin])
    r13 <- cor(df[, pname], df[, clin])
    r23 <- cor(df[, gname], df[, pname])
    gp_df[i, 'r12'] <- r12
    gp_df[i, 'r13'] <- r13
    gp_df[i, 'r23'] <- r23
    res <- r.test(n = nrow(df), r12 = r12, r13 = r13, r23 = r23)
    gp_df[i, 't'] <- res$t
    gp_df[i, 'p'] <- res$p
}
gp_df$p_fdr <- p.adjust(gp_df$p, method = 'fdr')
gp_df[gp_df$p_fdr < .05, ] #none significant

# B. Personalized versus intersection
pi_df <- data.frame(clins = rep(c('rrs_sum', 'bdi_sum', 'punishment', 'reward'), 17),
                    nets = c(rep('visuala', 4), rep('visualb', 4), rep('somatomotora', 4),
                             rep('somatomotorb', 4), rep('dorsalattentiona', 4),
                             rep('dorsalattentionb', 4), rep('saliencea', 4),
                             rep('salienceb', 4), rep('limbica', 4), rep('limbicb', 4),
                             rep('controla', 4), rep('controlb', 4), rep('controlc', 4), 
                             rep('defaulta', 4), rep('defaultb', 4), rep('defaultc', 4), 
                             rep('temporalparietal', 4)),
                    r12 = NA, r13 = NA, r23 = NA, t = NA, p = NA)
for (i in 1:nrow(pi_df)) {
    clin <- pi_df[i, 'clins']
    net <- pi_df[i, 'nets']
    pname <- paste0('FC_pers_', net, '_pos')
    iname <- paste0('FC_int_', net, '_pos')
    r12 <- cor(df[, pname], df[, clin])
    r13 <- cor(df[, iname], df[, clin])
    r23 <- cor(df[, pname], df[, iname])
    pi_df[i, 'r12'] <- r12
    pi_df[i, 'r13'] <- r13
    pi_df[i, 'r23'] <- r23
    res <- r.test(n = nrow(df), r12 = r12, r13 = r13, r23 = r23)
    pi_df[i, 't'] <- res$t
    pi_df[i, 'p'] <- res$p
}
pi_df$p_fdr <- p.adjust(pi_df$p, method = 'fdr')
pi_df[pi_df$p_fdr < .05, ] #none significant
