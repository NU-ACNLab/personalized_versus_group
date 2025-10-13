### This script conducts cross-validation to select a 
### penalty parameter that minimizes out of sample error
### selectively penalizing only the covariates (so not
### the FC estimates)

library(glmnet)

# Load data




# Ridge regression
penalized_columns <- c(0, 1, 1, 1, 1)

cv_ridge <- cv.glmnet(X, y, alpha = 0, penalty.factor = penalty_factors)
best_lambda <- cv_ridge$lambda.min
coef(ridge_model, s = best_lambda)