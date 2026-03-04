### This script conducts the pairwise comparisons and
### generates a plot with these results
###
### Ellyn Butler
### August 17, 2025 - March 3, 2026

set.seed(1234)

# Load libraries
library(ggplot2)
library(ggpubr)
library(psych)
library(dplyr)
library(tidyverse)
library(colorspace)
library(latex2exp)

# Load data
df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/combined/combined_2025-12-13.csv')

##### A. Group versus intersection
group <- names(df)[grep('group', names(df))]
int <- gsub('group', 'int', group)

gi_df <- data.frame(Clinical = c(rep('rrs_sum', length(group)), 
                              rep('bdi_sum', length(group)), 
                              rep('punishment', length(group)), 
                              rep('reward', length(group))),
                    group = rep(group, 4),
                    int = rep(int, 4),
                    r12 = NA, r13 = NA, r23 = NA, t = NA, p = NA)
for (i in 1:nrow(gi_df)) {
    clin <- gi_df[i, 'Clinical']
    gname <- gi_df[i, 'group']
    iname <- gi_df[i, 'int']
    r12 <- cor(df[, gname], df[, clin])
    r13 <- cor(df[, iname], df[, clin])
    r23 <- cor(df[, gname], df[, iname])
    gi_df[i, 'r12'] <- r12
    gi_df[i, 'r13'] <- r13
    gi_df[i, 'r23'] <- r23
    res <- r.test(n = nrow(df), r12 = r12, r13 = r13, r23 = r23)
    gi_df[i, 't'] <- res$t
    gi_df[i, 'p'] <- res$p
}
gi_df$p_fdr <- p.adjust(gi_df$p, method = 'fdr')
gi_df[gi_df$p_fdr < .05, ] #none significant
gi_df[gi_df$p < .05, ] #25
gi_df[gi_df$p < .01, ] #2

gi_df$Comparison <- 'Group vs. Intersection' #'Group vs. Int.'

gi_df2 <- gi_df[gi_df$p < .01, ]

gi_group_df2 <- gi_df2[, c('Comparison', 'Clinical', 'int', 'r12')]
gi_group_df2$Method <- 'Group'
gi_group_df2 <- gi_group_df2[, c('Comparison', 'Method', 'Clinical', 'int', 'r12')]
names(gi_group_df2) <- c('Comparison', 'Method', 'Clinical', 'Connectivity', 'r')

gi_int_df2 <- gi_df2[, c('Comparison', 'Clinical', 'int', 'r13')]
gi_int_df2$Method <- 'Intersection'
gi_int_df2 <- gi_int_df2[, c('Comparison', 'Method', 'Clinical', 'int', 'r13')]
names(gi_int_df2) <- c('Comparison', 'Method', 'Clinical', 'Connectivity', 'r')

# Is the magnitude of the association between intersection and clinical
# larger than the magnitude of the association between group and clinical
# on average? Significant?

# Across clinical
mean(abs(gi_df$r13) - abs(gi_df$r12))

gi_df$diff_abs <- abs(gi_df$r13) - abs(gi_df$r12)

# Permutations
gi_perm_df <- data.frame('rrs_sum' = rep(NA, 10000),
                         'bdi_sum' = rep(NA, 10000),
                         'punishment' = rep(NA, 10000),
                         'reward' = rep(NA, 10000))
# Loop through each permutation
for (i in 1:nrow(gi_perm_df)) {
    df2 <- df
    
    # Keep swap the same within an iteration
    swap <- runif(nrow(df2)) < 0.5

    # Loop through the clinical variables
    for (clin in names(gi_perm_df)) {
        r12_i <- rep(NA, length(group))
        r13_i <- rep(NA, length(int))
        # Loop through the FC variables
        for (j in 1:length(group)) {
            # Get the group and intersection FC variable names
            gname <- group[j]
            pname <- int[j]

            # Permute values
            df2[swap, c(gname, iname)] <- df[swap, c(iname, gname)]

            # Calculate permuted correlations
            r12_i[j] <- cor(df2[, gname], df2[, clin])
            r13_i[j] <- cor(df2[, iname], df2[, clin])
        } 
        # Get the mean of the difference of the absolute values
        gi_perm_df[i, clin] <- mean(abs(r13_i) - abs(r12_i))
    }
}

# Ruminative coping style
gi_r_m <- mean(abs(gi_df[gi_df$Clinical == 'rrs_sum', 'r13']) - abs(gi_df[gi_df$Clinical == 'rrs_sum', 'r12']))
summary(gi_perm_df$rrs_sum)

print('GI Ruminative coping style p = ')
sum(gi_r_m > gi_perm_df$rrs_sum)/nrow(gi_perm_df) # it is more extreme (more negative) than all but 7 of the permuted values
# p = 0.007

# Depression
gi_d_m <- mean(abs(gi_df[gi_df$Clinical == 'bdi_sum', 'r13']) - abs(gi_df[gi_df$Clinical == 'bdi_sum', 'r12']))
summary(gi_perm_df$bdi_sum)

print('GI Depression p = ')
sum(gi_d_m < gi_perm_df$bdi_sum)/nrow(gi_perm_df) # it is more extreme (more positive) than all of the permuted values
# p < 0.001

# Sensitivity to Punishment
gi_sp_m <- mean(abs(gi_df[gi_df$Clinical == 'punishment', 'r13']) - abs(gi_df[gi_df$Clinical == 'punishment', 'r12']))
summary(gi_perm_df$punishment)

print('GI Punishment p = ')
sum(gi_sp_m < gi_perm_df$punishment)/nrow(gi_perm_df) # it is more extreme (greater) than all but 3 of the permuted values
# p = 0.003

# Sensitivity to Reward
gi_sr_m <- mean(abs(gi_df[gi_df$Clinical == 'reward', 'r13']) - abs(gi_df[gi_df$Clinical == 'reward', 'r12']))
summary(gi_perm_df$reward)

print('GI Reward p = ')
sum(gi_sr_m < gi_perm_df$reward)/nrow(gi_perm_df) # it is more extreme (more positive) than all of the permuted values
# p < 0.001

##### B. Personalized versus intersection
pers <- gsub('group', 'pers', group)

pi_df <- data.frame(Clinical = c(rep('rrs_sum', length(group)), 
                              rep('bdi_sum', length(group)), 
                              rep('punishment', length(group)), 
                              rep('reward', length(group))),
                    pers = rep(pers, 4),
                    int = rep(int, 4),
                    r12 = NA, r13 = NA, r23 = NA, t = NA, p = NA)
for (i in 1:nrow(pi_df)) {
    clin <- pi_df[i, 'Clinical']
    pname <- pi_df[i, 'pers']
    iname <- pi_df[i, 'int']
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
pi_df[pi_df$p < .05, ] #46
pi_df[pi_df$p < .01, ] #13

pi_df$Comparison <- 'Personalized vs. Intersection'

pi_df2 <- pi_df[pi_df$p < .01, ]

pi_pers_df2 <- pi_df2[, c('Comparison', 'Clinical', 'int', 'r12')]
pi_pers_df2$Method <- 'Personalized'
pi_pers_df2 <- pi_pers_df2[, c('Comparison', 'Method', 'Clinical', 'int', 'r12')]
names(pi_pers_df2) <- c('Comparison', 'Method', 'Clinical', 'Connectivity', 'r')

pi_int_df2 <- pi_df2[, c('Comparison', 'Clinical', 'int', 'r13')]
pi_int_df2$Method <- 'Intersection'
pi_int_df2 <- pi_int_df2[, c('Comparison', 'Method', 'Clinical', 'int', 'r13')]
names(pi_int_df2) <- c('Comparison', 'Method', 'Clinical', 'Connectivity', 'r')


# Is the magnitude of the association between personalized and clinical
# larger than the magnitude of the association between intersection and clinical
# on average? Significant?

# Across clinical
mean(abs(pi_df$r13) - abs(pi_df$r12))

pi_df$diff_abs <- abs(pi_df$r13) - abs(pi_df$r12)

# Permutations
pi_perm_df <- data.frame('rrs_sum' = rep(NA, 10000),
                         'bdi_sum' = rep(NA, 10000),
                         'punishment' = rep(NA, 10000),
                         'reward' = rep(NA, 10000))
# Loop through each permutation
for (i in 1:nrow(pi_perm_df)) {
    df2 <- df
    
    # Keep swap the same within an iteration
    swap <- runif(nrow(df2)) < 0.5

    # Loop through the clinical variables
    for (clin in names(pi_perm_df)) {
        r12_i <- rep(NA, length(pers))
        r13_i <- rep(NA, length(int))
        # Loop through the FC variables
        for (j in 1:length(pers)) {
            # Get the group and intersection FC variable names
            pname <- pers[j]
            iname <- int[j]

            # Permute values
            df2[swap, c(pname, iname)] <- df[swap, c(iname, pname)]

            # Calculate permuted correlations
            r12_i[j] <- cor(df2[, pname], df2[, clin])
            r13_i[j] <- cor(df2[, iname], df2[, clin])
        } 
        # Get the mean of the difference of the absolute values
        pi_perm_df[i, clin] <- mean(abs(r13_i) - abs(r12_i))
    }
}

# Ruminative coping style
pi_r_m <- mean(abs(pi_df[pi_df$Clinical == 'rrs_sum', 'r13']) - abs(pi_df[pi_df$Clinical == 'rrs_sum', 'r12']))
summary(pi_perm_df$rrs_sum)

print('PI Ruminative coping style p = ')
sum(pi_r_m < pi_perm_df$rrs_sum)/nrow(pi_perm_df) # 
# p = 0.001

# Depression
pi_d_m <- mean(abs(pi_df[pi_df$Clinical == 'bdi_sum', 'r13']) - abs(pi_df[pi_df$Clinical == 'bdi_sum', 'r12']))
summary(pi_perm_df$bdi_sum)

print('PI Depression p = ')
sum(pi_d_m > pi_perm_df$bdi_sum)/nrow(pi_perm_df) # 
# p = 0.003

# Sensitivity to Punishment
pi_sp_m <- mean(abs(pi_df[pi_df$Clinical == 'punishment', 'r13']) - abs(pi_df[pi_df$Clinical == 'punishment', 'r12']))
summary(pi_perm_df$punishment)

print('PI Punishment p = ')
sum(pi_sp_m < pi_perm_df$punishment)/nrow(pi_perm_df) # 
# p < 0.001

# Sensitivity to Reward
pi_sr_m <- mean(abs(pi_df[pi_df$Clinical == 'reward', 'r13']) - abs(pi_df[pi_df$Clinical == 'reward', 'r12']))
summary(pi_perm_df$reward)

print('PI Reward p = ')
sum(pi_sr_m > pi_perm_df$reward)/nrow(pi_perm_df) # 
# p < 0.001

##### C. Group versus Personalized
gp_df <- data.frame(Clinical = c(rep('rrs_sum', length(group)), 
                              rep('bdi_sum', length(group)), 
                              rep('punishment', length(group)), 
                              rep('reward', length(group))),
                    group = rep(group, 4),
                    pers = rep(pers, 4),
                    r12 = NA, r13 = NA, r23 = NA, t = NA, p = NA)
for (i in 1:nrow(gp_df)) {
    clin <- gp_df[i, 'Clinical']
    gname <- gp_df[i, 'group']
    pname <- gp_df[i, 'pers']
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
gp_df[gp_df$p_fdr < .05, ] # 0
gp_df[gp_df$p < .05, ] # 39
gp_df[gp_df$p < .01, ] #5

gp_df$Comparison <- 'Group vs. Personalized'

gp_df2 <- gp_df[gp_df$p < .01, ]

gp_group_df2 <- gp_df2[, c('Comparison', 'Clinical', 'pers', 'r12')]
gp_group_df2$Method <- 'Group'
gp_group_df2 <- gp_group_df2[, c('Comparison', 'Method', 'Clinical', 'pers', 'r12')]
names(gp_group_df2) <- c('Comparison', 'Method', 'Clinical', 'Connectivity', 'r')

gp_pers_df2 <- gp_df2[, c('Comparison', 'Clinical', 'pers', 'r13')]
gp_pers_df2$Method <- 'Personalized'
gp_pers_df2 <- gp_pers_df2[, c('Comparison', 'Method', 'Clinical', 'pers', 'r13')]
names(gp_pers_df2) <- c('Comparison', 'Method', 'Clinical', 'Connectivity', 'r')


# Is the magnitude of the association between group and clinical
# larger than the magnitude of the association between personalized and clinical
# on average? Significant?

# Across clinical
mean(abs(gp_df$r13) - abs(gp_df$r12))

gp_df$diff_abs <- abs(gp_df$r13) - abs(gp_df$r12)

# Permutations
gp_perm_df <- data.frame('rrs_sum' = rep(NA, 10000),
                         'bdi_sum' = rep(NA, 10000),
                         'punishment' = rep(NA, 10000),
                         'reward' = rep(NA, 10000))
# Loop through each permutation
for (i in 1:nrow(gp_perm_df)) {
    df2 <- df
    
    # Keep swap the same within an iteration
    swap <- runif(nrow(df2)) < 0.5

    # Loop through the clinical variables
    for (clin in names(gp_perm_df)) {
        r12_i <- rep(NA, length(group))
        r13_i <- rep(NA, length(pers))
        # Loop through the FC variables
        for (j in 1:length(group)) {
            # Get the group and intersection FC variable names
            gname <- group[j]
            pname <- pers[j]

            # Permute values
            df2[swap, c(gname, pname)] <- df[swap, c(pname, gname)]

            # Calculate permuted correlations
            r12_i[j] <- cor(df2[, gname], df2[, clin])
            r13_i[j] <- cor(df2[, pname], df2[, clin])
        } 
        # Get the mean of the difference of the absolute values
        gp_perm_df[i, clin] <- mean(abs(r13_i) - abs(r12_i))
    }
}

# Ruminative coping style
gp_r_m <- mean(abs(gp_df[gp_df$Clinical == 'rrs_sum', 'r13']) - abs(gp_df[gp_df$Clinical == 'rrs_sum', 'r12']))
summary(gp_perm_df$rrs_sum)

print('GP Ruminative coping style p = ')
sum(gp_r_m > gp_perm_df$rrs_sum)/nrow(gp_perm_df) # 
# p < 0.001

# Depression
gp_d_m <- mean(abs(gp_df[gp_df$Clinical == 'bdi_sum', 'r13']) - abs(gp_df[gp_df$Clinical == 'bdi_sum', 'r12']))
summary(gp_perm_df$bdi_sum)

print('GP Depression p = ')
sum(gp_d_m < gp_perm_df$bdi_sum)/nrow(gp_perm_df) # 
# p < 0.001

# Sensitivity to Punishment
gp_sp_m <- mean(abs(gp_df[gp_df$Clinical == 'punishment', 'r13']) - abs(gp_df[gp_df$Clinical == 'punishment', 'r12']))
summary(gp_perm_df$punishment)

print('GP Punishment p = ')
sum(gp_sp_m < gp_perm_df$punishment)/nrow(gp_perm_df) # 
# p = 0.614

# Sensitivity to Reward
gp_sr_m <- mean(abs(gp_df[gp_df$Clinical == 'reward', 'r13']) - abs(gp_df[gp_df$Clinical == 'reward', 'r12']))
summary(gp_perm_df$reward)

print('GP Reward p = ')
sum(gp_sr_m < gp_perm_df$reward)/nrow(gp_perm_df) # 
# p < 0.001

# D. Plotting - permutations
perm_df <- data.frame(
    Comparison  = c(rep('Group vs. Intersection', 4000), 
                    rep('Personalized vs. Intersection', 4000), 
                    rep('Group vs. Personalized', 4000)),
    Clinical    = c(rep(c(rep('Depression', 1000), 
                    rep('Rumination', 1000), 
                    rep('Punishment', 1000), 
                    rep('Reward', 1000)), 3)),
    Value       = c(gi_perm_df$bdi_sum, gi_perm_df$rrs_sum, 
                    gi_perm_df$punishment, gi_perm_df$reward,
                    pi_perm_df$bdi_sum, pi_perm_df$rrs_sum, 
                    pi_perm_df$punishment, pi_perm_df$reward,
                    gp_perm_df$bdi_sum, gp_perm_df$rrs_sum, 
                    gp_perm_df$punishment, gp_perm_df$reward)
)

# Group vs. Intersection
gi_hline_data <- data.frame(
                Clinical = c('Depression', 'Rumination', 'Punishment', 'Reward'),
                Value = c(gi_d_m, gi_r_m, gi_sp_m, gi_sr_m)
)
gi_hline_data$Clinical <- ordered(gi_hline_data$Clinical, levels = c('Depression', 'Rumination', 'Punishment', 'Reward'))

gi_summ_df <- perm_df[perm_df$Comparison == 'Group vs. Intersection', ]
gi_summ_df$Clinical <- ordered(gi_summ_df$Clinical, levels = c('Depression', 'Rumination', 'Punishment', 'Reward'))

gi_perm <- ggplot(gi_summ_df, aes(x = Value)) + theme_linedraw() +
            geom_histogram() + facet_grid(cols = vars(Clinical)) + 
            geom_vline(data = gi_hline_data, aes(xintercept = Value),
                       color = 'blue',
                       linetype = 'dashed',
                       linewidth = 1)

png(paste0('~/Documents/Northwestern/projects/personalized_versus_group/plots/gi_perm_', format(Sys.Date()),'.png'), width = 3200, height = 1000, res = 300)
gi_perm
dev.off()

# Personalized vs. Intersection
pi_hline_data <- data.frame(
                Clinical = c('Depression', 'Rumination', 'Punishment', 'Reward'),
                Value = c(pi_d_m, pi_r_m, pi_sp_m, pi_sr_m)
)
pi_hline_data$Clinical <- ordered(pi_hline_data$Clinical, levels = c('Depression', 'Rumination', 'Punishment', 'Reward'))

pi_summ_df <- perm_df[perm_df$Comparison == 'Personalized vs. Intersection', ]
pi_summ_df$Clinical <- ordered(pi_summ_df$Clinical, levels = c('Depression', 'Rumination', 'Punishment', 'Reward'))

pi_perm <- ggplot(pi_summ_df, aes(x = Value)) + theme_linedraw() +
            geom_histogram() + facet_grid(cols = vars(Clinical)) + 
            geom_vline(data = pi_hline_data, aes(xintercept = Value),
                       color = 'blue',
                       linetype = 'dashed',
                       linewidth = 1)

png(paste0('~/Documents/Northwestern/projects/personalized_versus_group/plots/pi_perm_', format(Sys.Date()),'.png'), width = 3200, height = 1000, res = 300)
pi_perm
dev.off()

# Group vs. Personalized
gp_hline_data <- data.frame(
                Clinical = c('Depression', 'Rumination', 'Punishment', 'Reward'),
                Value = c(gp_d_m, gp_r_m, gp_sp_m, gp_sr_m)
)
gp_hline_data$Clinical <- ordered(gp_hline_data$Clinical, levels = c('Depression', 'Rumination', 'Punishment', 'Reward'))

gp_summ_df <- perm_df[perm_df$Comparison == 'Group vs. Personalized', ]
gp_summ_df$Clinical <- ordered(gp_summ_df$Clinical, levels = c('Depression', 'Rumination', 'Punishment', 'Reward'))

gp_perm <- ggplot(gp_summ_df, aes(x = Value)) + theme_linedraw() +
            geom_histogram() + facet_grid(cols = vars(Clinical)) + 
            geom_vline(data = gp_hline_data, aes(xintercept = Value),
                       color = 'blue',
                       linetype = 'dashed',
                       linewidth = 1)

png(paste0('~/Documents/Northwestern/projects/personalized_versus_group/plots/gp_perm_', format(Sys.Date()),'.png'), width = 3200, height = 1000, res = 300)
gp_perm
dev.off()

# E. Plotting - distributions
names(gi_df) <- c('Clinical', 'type1', 'type2', 'r12', 'r13', 'r23',
                  't', 'p', 'p_fdr', 'Comparison', 'diff_abs')
names(pi_df) <- c('Clinical', 'type1', 'type2', 'r12', 'r13', 'r23',
                  't', 'p', 'p_fdr', 'Comparison', 'diff_abs')
names(gp_df) <- c('Clinical', 'type1', 'type2', 'r12', 'r13', 'r23',
                  't', 'p', 'p_fdr', 'Comparison', 'diff_abs')

# Group vs. Intersection
gi_long <- data.frame(Correlations = c(rep('Group', nrow(gi_df)), rep('Intersection', nrow(gi_df)), 
                rep('Difference of the Absolute Values', nrow(gi_df))), 
                Values = c(gi_df$r12, gi_df$r13, gi_df$diff_abs))
gi_long$Correlations <- ordered(gi_long$Correlations, c('Group', 'Intersection', 'Difference of the Absolute Values'))

gi_plot <- ggplot(gi_long, aes(Values, fill = Correlations)) + theme_linedraw() + 
            geom_density(alpha = 0.5) + 
            scale_fill_manual(values = c('#b7baeb', '#b7ebc5', 'black')) + 
            theme(legend.position = 'bottom') +  
            xlim(c(-.2, .2)) + ylim(c(0, 18))
            
png(paste0('~/Documents/Northwestern/projects/personalized_versus_group/plots/gi_', format(Sys.Date()),'.png'), width = 2100, height = 1200, res = 380)
gi_plot
dev.off()

# Personalized vs. Intersection
pi_long <- data.frame(Correlations = c(rep('Personalized', nrow(pi_df)), rep('Intersection', nrow(pi_df)), 
                rep('Difference of the Absolute Values', nrow(pi_df))), 
                Values = c(pi_df$r12, pi_df$r13, pi_df$diff_abs)) 
pi_long$Correlations <- ordered(pi_long$Correlations, c('Personalized', 'Intersection', 'Difference of the Absolute Values'))

pi_plot <- ggplot(pi_long, aes(Values, fill = Correlations)) + theme_linedraw() + 
            geom_density(alpha = 0.5) + 
            scale_fill_manual(values = c('#ebe6b7', '#b7ebc5', 'black')) + 
            theme(legend.position = 'bottom') + 
            xlim(c(-.2, .2)) + ylim(c(0, 18))

png(paste0('~/Documents/Northwestern/projects/personalized_versus_group/plots/pi_', format(Sys.Date()),'.png'), width = 2100, height = 1200, res = 380)
pi_plot
dev.off()

# Group vs. Personalized
gp_long <- data.frame(Correlations = c(rep('Group', nrow(gp_df)), rep('Personalized', nrow(gp_df)), 
                rep('Difference of the Absolute Values', nrow(gp_df))), 
                Values = c(gp_df$r12, gp_df$r13, gp_df$diff_abs)) 
gp_long$Correlations <- ordered(gp_long$Correlations, c('Group', 'Personalized', 'Difference of the Absolute Values'))

gp_plot <- ggplot(gp_long, aes(Values, fill = Correlations)) + theme_linedraw() + 
            geom_density(alpha = 0.5) + 
            scale_fill_manual(values = c('#b7baeb', '#ebe6b7', 'black')) + 
            theme(legend.position = 'bottom') + 
            xlim(c(-.2, .2)) + ylim(c(0, 18))

png(paste0('~/Documents/Northwestern/projects/personalized_versus_group/plots/gp_', format(Sys.Date()),'.png'), width = 2100, height = 1200, res = 380)
gp_plot
dev.off()

# F. Plotting - distributions 2

# Group vs. Intersection
gi_long <- data.frame(Correlations = c(rep('Group', nrow(gi_df)), rep('Intersection', nrow(gi_df))), 
                Clinical = c(gi_df$Clinical, gi_df$Clinical),
                Values = c(abs(gi_df$r12), abs(gi_df$r13)))
gi_long$Clinical <- recode(gi_long$Clinical, 'rrs_sum' = 'Rumination', 
                           'bdi_sum' = 'Depression', 'punishment' = 'Punishment',
                           'reward' = 'Reward')
gi_long$Clinical <- ordered(gi_long$Clinical, c('Depression', 'Rumination', 'Punishment', 'Reward'))                         
gi_long$Correlations <- ordered(gi_long$Correlations, c('Group', 'Intersection'))

vlines <- gi_long %>%
    group_by(Clinical, Correlations) %>%
    summarise(
        xintercept = mean(abs(Values)),
        .groups = "drop"
    )
gi_plot <- ggplot(gi_long, aes(Values, fill = Correlations)) + 
    theme_classic() + 
    facet_wrap(~ Clinical, ncol = 2) + 
    geom_histogram(alpha = 0.5, bins = 50, color = 'grey', position = 'identity') + 
    geom_vline(
        data = vlines,
        aes(xintercept = xintercept, color = Correlations),
        linetype = 'dashed',
        linewidth = 1
    ) +
    scale_fill_manual(values = c(Group = '#b7baeb', Intersection = '#b7ebc5')) + 
    scale_color_manual(values = c(Group = darken('#b7baeb', 0.3), Intersection = darken('#b7ebc5', 0.3))) +
    theme(
        legend.position = 'none',
        plot.margin = margin(t = 5.5, r = 10, b = 5.5, l = 5.5),
        axis.text.x = element_text(size = 6)
    ) + 
    xlab(TeX('$|r_{G\\,\\or\\,I,\\,\\Clinical}|$'))
    
png(paste0('~/Documents/Northwestern/projects/personalized_versus_group/plots/gi_2_', format(Sys.Date()),'.png'), width = 1500, height = 1100, res = 380)
gi_plot
dev.off()

# Personalized vs. Intersection
pi_long <- data.frame(Correlations = c(rep('Personalized', nrow(pi_df)), rep('Intersection', nrow(pi_df))), 
                Clinical = c(pi_df$Clinical, pi_df$Clinical),
                Values = c(abs(pi_df$r12), abs(pi_df$r13)))
pi_long$Clinical <- recode(pi_long$Clinical, 'rrs_sum' = 'Rumination', 
                           'bdi_sum' = 'Depression', 'punishment' = 'Punishment',
                           'reward' = 'Reward')
pi_long$Clinical <- ordered(pi_long$Clinical, c('Depression', 'Rumination', 'Punishment', 'Reward'))                         
pi_long$Correlations <- ordered(pi_long$Correlations, c('Personalized', 'Intersection')) 

vlines <- pi_long %>%
    group_by(Clinical, Correlations) %>%
    summarise(
        xintercept = mean(abs(Values)),
        .groups = "drop"
    )
pi_plot <- ggplot(pi_long, aes(Values, fill = Correlations)) + 
    theme_classic() + 
    facet_wrap(~ Clinical, ncol = 2) + 
    geom_histogram(alpha = 0.5, bins = 50, color = 'grey', position = 'identity') + 
    geom_vline(
        data = vlines,
        aes(xintercept = xintercept, color = Correlations),
        linetype = 'dashed',
        linewidth = 1
    ) +
    scale_fill_manual(values = c(Personalized = '#ebe6b7', Intersection = '#b7ebc5')) + 
    scale_color_manual(values = c(Personalized = darken('#ebe6b7', 0.3), Intersection = darken('#b7ebc5', 0.3))) +
    theme(
        legend.position = 'none',
        plot.margin = margin(t = 5.5, r = 10, b = 5.5, l = 5.5),
        axis.text.x = element_text(size = 6)
    ) + 
    xlab(TeX('$|r_{P\\,\\or\\,I,\\,\\Clinical}|$'))

png(paste0('~/Documents/Northwestern/projects/personalized_versus_group/plots/pi_2_', format(Sys.Date()),'.png'),width = 1500, height = 1100, res = 380)
pi_plot
dev.off()

# Group vs. Personalized
gp_long <- data.frame(Correlations = c(rep('Group', nrow(gp_df)), rep('Personalized', nrow(gp_df))), 
                Clinical = c(gp_df$Clinical, gp_df$Clinical),
                Values = c(abs(gp_df$r12), abs(gp_df$r13)))
gp_long$Clinical <- recode(gp_long$Clinical, 'rrs_sum' = 'Rumination', 
                           'bdi_sum' = 'Depression', 'punishment' = 'Punishment',
                           'reward' = 'Reward')
gp_long$Clinical <- ordered(gp_long$Clinical, c('Depression', 'Rumination', 'Punishment', 'Reward'))                         
gp_long$Correlations <- ordered(gp_long$Correlations, c('Group', 'Personalized')) 

vlines <- gp_long %>%
    group_by(Clinical, Correlations) %>%
    summarise(
        xintercept = mean(abs(Values)),
        .groups = "drop"
    )
gp_plot <- ggplot(gp_long, aes(Values, fill = Correlations)) + 
    theme_classic() + 
    facet_wrap(~ Clinical, ncol = 2) + 
    geom_histogram(alpha = 0.5, bins = 50, color = 'grey', position = 'identity') + 
    geom_vline(
        data = vlines,
        aes(xintercept = xintercept, color = Correlations),
        linetype = 'dashed',
        linewidth = 1
    ) +
    scale_fill_manual(values = c(Group = '#b7baeb', Personalized = '#ebe6b7')) + 
    scale_color_manual(values = c(Group = darken('#b7baeb', 0.3), Personalized = darken('#ebe6b7', 0.3))) +
    theme(
        legend.position = 'none',
        plot.margin = margin(t = 5.5, r = 10, b = 5.5, l = 5.5),
        axis.text.x = element_text(size = 6)
    ) + 
    xlab(TeX('$|r_{G\\,\\or\\,P,\\,\\Clinical}|$'))

png(paste0('~/Documents/Northwestern/projects/personalized_versus_group/plots/gp_2_', format(Sys.Date()),'.png'), width = 1500, height = 1100, res = 380)
gp_plot
dev.off()

save.image(file = paste0('~/Documents/Northwestern/projects/personalized_versus_group/data/session_', format(Sys.Date()), '.RData'))