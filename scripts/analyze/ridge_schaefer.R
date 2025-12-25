### This script runs an exploratory analysis of ridge regression
### using solely the FC values from the Schaefer 400 atlas
### predicting the four clinical metrics.
###
### Ellyn Butler
### December 24, 2025 - December 25, 2025

set.seed(1234)

library(glmnet)

indir <- '/projects/b1108/projects/personalized_versus_group/data/processed/neuroimaging/tabulated/'
#comp_df <- read.csv(paste0(indir, 'spatial_components_1perc_2025-12-14.csv'))
clin_df <- read.csv(paste0(indir, 'prior_subjects_2025-09-19.csv'))
net_df <- read.csv(paste0(indir, 'schaefer_400_2025-12-24.csv'))

df <- merge(clin_df, net_df)

# Select variables to include in model
FCs <- names(df)[grepl('FC', names(df))]
FCs <- unique(FCs)

# Regression
clins <- c('rrs_sum', 'bdi_sum', 'punishment', 'reward')

lambda_seq <- 10^seq(3, -3, by = -.1)
X <- scale(as.matrix(df[, FCs]))

for (clin in clins) {
        y <- scale(as.matrix(df[, clin]))
        #penalized_columns <- c(rep(0, length(FCs)), rep(1, length(exps)), rep(1, length(comps)))
        
        ind <- sample(1:324)

        lst <- split(ind, cut(seq_along(ind), 5, labels = FALSE))
        full_rsqs <- c()
        for (i in 1:5) {
            X_train <- X[unlist(lst[(1:5)[-i]], use.names = FALSE), ]
            y_train <- y[unlist(lst[(1:5)[-i]], use.names = FALSE), ]
            X_test <- X[lst[[i]], ]
            y_test <- y[lst[[i]]]
            # Full model
            cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, lambda = lambda_seq)
                        #penalty.factor = penalized_columns)
            best_lambda <- cv_ridge$lambda.min # rrs_sum chose 1000, 
            print(paste0(clin, ' ', best_lambda))

            ridge_model <- cv_ridge$glmnet.fit
            #coef(ridge_model, s = best_lambda)
            #head(ridge_model)

            # Make predictions with the best model (replace s = best_lambda with the appropriate value)
            y_predicted <- predict(ridge_model, s = best_lambda, newx = X_test) 
            
             # Calculate SST
            sst <- sum((y_test - mean(y_test))^2)

            # Calculate SSE
            sse <- sum((y_predicted - y_test)^2)

            # Get rsq
            full_rsq <- 1 - (sse/sst)
            full_rsqs <- c(full_rsqs, full_rsq)
        }
        assign(paste0('full_rsqs_', clin), full_rsqs)
        print(paste0('A ridge model with ', clin, 
            ' as the outcome measure has an average out of sample R squared value of ', 
            mean(full_rsqs)))
}
