### This script runs an exploratory analysis of ridge regression
### using solely the FC values from the Schaefer 400 atlas
### predicting the four clinical metrics.
###
### Ellyn Butler
### December 24, 2025

lambda_seq <- 10^seq(3, -3, by = -.1)

X <- scale(as.matrix(df[, c(FCs, exps, comps)]))
        y <- scale(as.matrix(df[, clin]))
        penalized_columns <- c(rep(0, length(FCs)), rep(1, length(exps)), rep(1, length(comps)))


# Full model
        cv_ridge <- cv.glmnet(X, y, alpha = 0, lambda = lambda_seq,
                    penalty.factor = penalized_columns)
        best_lambda <- cv_ridge$lambda.min #NOTE: Keeps picking the biggest one, even when expanded range of seq

        ridge_model <- cv_ridge$glmnet.fit
        coef(ridge_model, s = best_lambda)
        head(ridge_model)

        # Make predictions with the best model (replace s = best_lambda with the appropriate value)
        y_predicted <- predict(ridge_model, s = best_lambda, newx = X) #Can I use the old X here?

        # Calculate SST
        sst <- sum((y - mean(y))^2)

        # Calculate SSE
        sse <- sum((y_predicted - y)^2)
        full_rsq <- 1 - (sse/sst)
