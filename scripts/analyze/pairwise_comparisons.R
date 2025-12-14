### This script conducts the pairwise comparisons and
### generates a plot with these results
###
### Ellyn Butler
### August 17, 2025 - December 13, 2025

# Load libraries
library(ggplot2)
library(ggpubr)
library(psych)
library(dplyr)

# Load data
df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/combined/combined_2025-12-13.csv')

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
gi_df[gi_df$p < .05, ] #25
gi_df[gi_df$p < .01, ] #2

gi_df$Comparison <- 'Group vs. Int.'

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

# C. Group versus Personalized
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

# D. Plotting
comb_df <- rbind(
    gi_group_df2, gi_int_df2, 
    pi_pers_df2, pi_int_df2, 
    gp_group_df2, gp_pers_df2
)

comb_df$Clinical <- recode(comb_df$Clinical, 
                            'bdi_sum' = 'Depression',
                            'punishment' = 'Sensitivity to Punishment',
                            'reward' = 'Sensitivity to Reward',
                            'rrs_sum' = 'Ruminative Coping Style'
                        )

comb_df$Method <- ordered(comb_df$Method, c('Group', 'Intersection', 'Personalized'))

comb_df$Connectivity <- recode(comb_df$Connectivity, 
                            'FC_int_visualb_limbica_pos' = 'Visual B - Limbic A',   #
                            'FC_int_saliencea_pos' = 'Salience A',   #         
                            'FC_int_visuala_defaultc_pos' = 'Visual A - Default C',   #   
                            'FC_int_dorsalattentiona_defaultc_pos' = 'Dorsal Attention A - Default C', #
                            'FC_int_salienceb_defaultc_pos' = 'Salience B - Default C',   # 
                            'FC_int_defaultb_defaultc_pos' = 'Default B - Default C',    #
                            'FC_int_limbica_pos' = 'Limbic A',  #              
                            'FC_int_visuala_visualb_pos' = 'Visual A - Visual B',  #      
                            'FC_int_visuala_somatomotorb_pos' = 'Visual A - Somatomotor B', #
                            'FC_int_visualb_defaultb_pos' = 'Visual B - Default B',  #     
                            'FC_int_visualb_temporalparietal_pos' = 'Visual B - Temporal Parietal', #
                            'FC_int_dorsalattentiona_controlb_pos' = 'Dorsal Attention A - Control B', #
                            'FC_int_dorsalattentionb_temporalparietal_pos' = 'Dorsal Attention B - Temporal Parietal', #
                            'FC_int_limbica_controla_pos' = 'Limbic A - Control A', #
                            'FC_int_salienceb_defaultb_pos' = 'Salience B - Default B', #
                            'FC_pers_visualb_limbica_pos' = 'Visual B - Limbic A',
                            'FC_pers_somatomotorb_salienceb_pos' = 'Somatomotor B - Salience B',
                            'FC_pers_dorsalattentiona_defaultc_pos' = 'Dorsal Attention A - Default C',
                            'FC_pers_saliencea_pos' = 'Salience A', 
                            'FC_pers_limbica_pos' = 'Limbic A'
                        )

comb_df$Connectivity_Clinical <- interaction(comb_df$Connectivity, comb_df$Clinical, sep = " | ")

plot1 <- ggplot(comb_df, aes(
    x = Connectivity_Clinical,
    y = r,
    color = Clinical,
    shape = Method
  )) +
  theme_linedraw() +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_line(aes(group = Connectivity_Clinical), color = 'black', linewidth = 0.5) +
  geom_point(size = 5) +
  ylim(-0.2, 0.2) +
  facet_grid(cols = vars(Comparison), scales = 'free_x', space = 'free_x') +
  scale_shape_manual(values = c(15, 16, 18))+
  scale_x_discrete(
    guide = guide_axis(angle = 60),
    labels = c('Visual B - Limbic A',
               'Salience A',
               'Dorsal Attention A - Default C',
               'Somatomotor B - Salience B',
               'Visual B - Limbic A',
               'Salience A',
               'Limbic A',
               'Limbic A',
               'Default B - Default C',
               'Dornsal Attention A - Default C',
               "salience B - Default C",
               'Visual A- Default C',
               'Dorsal Attention A - Control B',
               'Dorsal Attention B - Temporal Parietal',
               'Limbic A - Control A',
               'Visual A - Visual B',
               'Salience B - Default B',
               'Visual A - Somatomotor B',
               'Visual B - Default B',
               'Visusal B - Temporal Parietal')
  ) +
  ylab('Correlation between Clinical and Connectivity') +
  xlab('Connectivity') + 
  theme(legend.position = 'bottom', legend.box = 'vertical')

png(paste0('~/Documents/Northwestern/projects/personalized_versus_group/plots/effect_size_comparisons_', format(Sys.Date()),'.png'), width = 3000, height = 1800, res = 300)
plot1
dev.off()
