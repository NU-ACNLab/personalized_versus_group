### This script conducts the pairwise comparisons and
### generates a plot with these results
###
### Ellyn Butler
### August 17, 2025 - September 29, 2025

# Load libraries
library(ggplot2)
library(ggpubr)
library(psych)
library(dplyr)

# Load data
df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/combined/combined_2025-09-29.csv')

# Descriptive
summary(df$age)
table(df$sex) #0 male, 1 female

# A. Group versus intersection
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
gi_df[gi_df$p < .05, ] #21
gi_df[gi_df$p < .01, ] #5

gi_df$Comparison <- 'Group vs. Intersection'

gi_df2 <- gi_df[gi_df$p < .01, ]

gi_group_df2 <- gi_df2[, c('Comparison', 'Clinical', 'int', 'r12')]
gi_group_df2$Method <- 'Group'
gi_group_df2 <- gi_group_df2[, c('Comparison', 'Method', 'Clinical', 'int', 'r12')]
names(gi_group_df2) <- c('Comparison', 'Method', 'Clinical', 'Connectivity', 'r')

gi_int_df2 <- gi_df2[, c('Comparison', 'Clinical', 'int', 'r13')]
gi_int_df2$Method <- 'Intersection'
gi_int_df2 <- gi_int_df2[, c('Comparison', 'Method', 'Clinical', 'int', 'r13')]
names(gi_int_df2) <- c('Comparison', 'Method', 'Clinical', 'Connectivity', 'r')

# B. Personalized versus intersection
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
pi_df[pi_df$p < .05, ] #37
pi_df[pi_df$p < .01, ] #5

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

# C. Plotting
comb_df <- rbind(
    gi_group_df2, gi_int_df2, pi_pers_df2, pi_int_df2
)

comb_df$Clinical <- recode(comb_df$Clinical, 'punishment' = 'Sensitivity to Punishment',
                            'reward' = 'Sensitivity to Reward',
                            'rrs_sum' = 'Ruminative Coping Style')

comb_df$Method <- ordered(comb_df$Method, c('Group', 'Intersection', 'Personalized'))

comb_df$Connectivity <- recode(comb_df$Connectivity, 
                            'FC_int_somatomotorb_defaultc_pos' = 'Somatomotor B - Default C',
                            'FC_int_somatomotorb_controla_pos' = 'Somatomotor B - Control A',
                            'FC_int_saliencea_controla_pos' = 'Salience A - Control A', 
                            'FC_int_salienceb_defaulta_pos' = 'Salience B - Default A',
                            'FC_int_controlc_defaultb_pos' = 'Control C - Default B',
                            'FC_int_dorsalattentiona_controlc_pos' = 'Dorsal Attention A - Control C',
                            'FC_int_visuala_defaulta_pos' = 'Visual A - Default A',
                            'FC_int_dorsalattentionb_controlb_pos' = 'Dorsal Attention B - Control B',
                            'FC_int_saliencea_defaultb_pos' = 'Salience A - Default B',
                            'FC_int_controla_pos' = 'Control A'
                        )

plot1 <- ggplot(comb_df, aes(x = Connectivity, y = r, color = Clinical, shape = Method)) +
            theme_linedraw() + geom_hline(yintercept = 0, linetype = 'dashed') + 
            geom_line(aes(group = Connectivity), color = 'black', linewidth = 0.5) +
            geom_point(size = 5, stat = 'identity') + ylim(-0.2, 0.2) + 
            facet_grid(cols = vars(Comparison), scales = 'free_x', space = 'free_x') + 
            scale_x_discrete(guide = guide_axis(angle = 45)) + 
            ylab('Correlation between Clinical and Connectivity') +
            theme(legend.position = 'bottom', legend.box = 'vertical') 

png(paste0('~/Documents/Northwestern/projects/personalized_versus_group/plots/effect_size_comparisons_', format(Sys.Date()),'.png'), width = 2500, height = 1800, res = 300)
plot1
dev.off()
