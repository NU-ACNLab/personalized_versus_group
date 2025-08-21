### This script conducts the pairwise comparisons and
### generates a plot with these results
###
### Ellyn Butler
### August 17, 2025 - 

# Load libraries
library(ggplot2)
library(ggpubr)
library(psych)

# Load data
df <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/combined/poster_2025-08-17.csv')

# Descriptive
summary(df$age)
table(df$sex) #0 male, 1 female

# A. Group versus personalized
group <- names(df)[grep('group', names(df))]
pers <- gsub('group', 'pers', group)

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
gp_df[gp_df$p_fdr < .05, ] #none significant
gp_df[gp_df$p < .05, ] #44
gp_df[gp_df$p < .01, ] #11

gp_df$Comparison <- 'GP'

gp_df2 <- gp_df[gp_df$p < .01, ]

gp_group_df2 <- gp_df2[, c('Comparison', 'Clinical', 'pers', 'r12')]
gp_group_df2$Method <- 'Group'
gp_group_df2 <- gp_group_df2[, c('Comparison', 'Method', 'Clinical', 'pers', 'r12')]
names(gp_group_df2) <- c('Comparison', 'Method', 'Clinical', 'Networks', 'r')

gp_pers_df2 <- gp_df2[, c('Comparison', 'Clinical', 'pers', 'r13')]
gp_pers_df2$Method <- 'Personalized'
gp_pers_df2 <- gp_pers_df2[, c('Comparison', 'Method', 'Clinical', 'pers', 'r13')]
names(gp_pers_df2) <- c('Comparison', 'Method', 'Clinical', 'Networks', 'r')

# B. Personalized versus intersection
int <- gsub('group', 'int', group)

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

pi_df$Comparison <- 'PI'

pi_df2 <- pi_df[pi_df$p < .01, ]

pi_pers_df2 <- pi_df2[, c('Comparison', 'Clinical', 'pers', 'r12')]
pi_pers_df2$Method <- 'Personalized'
pi_pers_df2 <- pi_pers_df2[, c('Comparison', 'Method', 'Clinical', 'pers', 'r12')]
names(pi_pers_df2) <- c('Comparison', 'Method', 'Clinical', 'Networks', 'r')

pi_int_df2 <- pi_df2[, c('Comparison', 'Clinical', 'pers', 'r13')]
pi_int_df2$Method <- 'Intersection'
pi_int_df2 <- pi_int_df2[, c('Comparison', 'Method', 'Clinical', 'pers', 'r13')]
names(pi_int_df2) <- c('Comparison', 'Method', 'Clinical', 'Networks', 'r')

# C. Plotting
comb_df <- rbind(
    gp_group_df2, gp_pers_df2, pi_pers_df2, pi_int_df2
)

comb_df$Networks <- recode()

plot1 <- ggplot(comb_df, aes(x = Networks, y = r, color = Clinical, shape = Method)) +
            theme_linedraw() + geom_point(stat = 'identity') + 
            facet_grid(cols = vars(Comparison)) + ylim(-0.2, 0.2) + 
            scale_x_discrete(guide = guide_axis(angle = 90)) 

