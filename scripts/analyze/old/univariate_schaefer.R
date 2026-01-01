### This script runs an exploratory analysis of correlations
### FDR-corrected of the association between all of the Schaefer
### FC variables and the four clinical metrics.
###
### Ellyn Butler
### December 31, 2025

# Load data
library(data.table)

indir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/'

clin_df <- fread(paste0(indir, 'prior_subjects_2025-09-19.csv'))
net_df  <- fread(paste0(indir, 'schaefer_400_2025-12-24.csv'))

# Merge
setkey(clin_df, 'subid')
setkey(net_df, 'subid')

df <- net_df[clin_df]   # left join, fast

# Extract matrices
FCs   <- grep('^17networks', names(df), value = TRUE)
clins <- c('rrs_sum', 'bdi_sum', 'punishment', 'reward')

X <- as.matrix(df[, FCs, with = FALSE])        # n × p (big)
Y <- as.matrix(df[, clins, with = FALSE])      # n × 4

# Vectorized correlations
cors <- cor(X, Y, use = 'pairwise.complete.obs')
# dim(cors) = length(FCs) × 4

# Vectorized p-values
n <- nrow(df)  

tvals <- cors * sqrt((n - 2) / (1 - cors^2))
pvals <- 2 * pt(abs(tvals), df = n - 2, lower.tail = FALSE)

# Compile results
res_df <- data.frame(
  FC = FCs,
  cor_rrs_sum       = cors[, 'rrs_sum'],
  p_rrs_sum         = pvals[, 'rrs_sum'],
  cor_bdi_sum       = cors[, 'bdi_sum'],
  p_bdi_sum         = pvals[, 'bdi_sum'],
  cor_punishment    = cors[, 'punishment'],
  p_punishment      = pvals[, 'punishment'],
  cor_reward        = cors[, 'reward'],
  p_reward          = pvals[, 'reward']
)

# FDR correction
res_df$p_fdr_rrs_sum    <- p.adjust(res_df$p_rrs_sum, method = 'fdr')
res_df$p_fdr_bdi_sum    <- p.adjust(res_df$p_bdi_sum, method = 'fdr')
res_df$p_fdr_punishment <- p.adjust(res_df$p_punishment, method = 'fdr')
res_df$p_fdr_reward     <- p.adjust(res_df$p_reward, method = 'fdr')

# Return significant results
subset(res_df, p_fdr_rrs_sum < 0.05) #0
subset(res_df, p_fdr_bdi_sum < 0.05) #0
subset(res_df, p_fdr_punishment < 0.05) #0
subset(res_df, p_fdr_reward < 0.05) #0

