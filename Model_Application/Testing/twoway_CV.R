#load libraries
library(quantreg)

#load data
#data.full <- readRDS()
single.data <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Dissertation_Git/Data_Generation/Data_Storage/single_data_091520.RData")
X <- single.data[[1]]$X
Y <- single.data[[1]]$Y
p <- single.data[[1]]$conditions$p

#ridge coefs for weighting
lambda.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
ridge.model <- cv.glmnet(x = X , y = Y , lambda = lambda.try , alpha = 0)
lambda.ridge.opt <- ridge.model$lambda.min
best.ridge.coefs <- predict(ridge.model , type = "coefficients" ,
                           s = lambda.ridge.opt)[-1]
#single adaptive lasso run with ridge weighting and nu = 1
adalasso.model <- cv.glmnet(X , Y , family = "gaussian" ,
                            lambda = lambda.try , penalty.factor = 1 / abs(best.ridge.coefs))
lambda.adalasso.opt <- adalasso.model$lambda.min
best.adalasso.coefs <- predict(adalasso.model , type = "coefficients" ,
                           s = lambda.adalasso.opt)[-1]
model <- list(full.model = adalasso.model , weights = 1 / abs(best.ridge.coefs) , 
              lambda = lambda.adalasso.opt , coefs = best.adalasso.coefs)
metrics_and_info <- list(weights = 1 / abs(best.ridge.coefs) , lambda = lambda.adalasso.opt , 
                         coefs = best.adalasso.coefs , 
                         mpe = adalasso.model$cvm[which(adalasso.model$lambda == lambda.adalasso.opt)] , 
                         mpe.sd = adalasso.model$cvsd[which(adalasso.model$lambda == lambda.adalasso.opt)] , 
                         fpr = length(which(best.adalasso.coefs[c(5:p)] != 0)) / length(best.adalasso.coefs[c(5:p)]) , 
                         fnr = length(which(best.adalasso.coefs[c(1:4)] == 0)) / length(best.adalasso.coefs[1:4]))
model
metrics_and_info

#repeat above process for multiple nu's
##grid of nu/gamma values to try
nu.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
##initialize list of best adalasso results from each nu/gamma
adalasso.nu.cv <- list()
for(i in 1:length(nu.try)) {
       seed <- sample(x = c(1:1000) , size = 1 , replace = FALSE)
       set.seed(seed)
       #ridge coefs for weighting
       lambda.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       ridge.model <- cv.glmnet(x = X , y = Y , lambda = lambda.try , alpha = 0)
       lambda.ridge.opt <- ridge.model$lambda.min
       best.ridge.coefs <- predict(ridge.model , type = "coefficients" ,
                                   s = lambda.ridge.opt)[-1]
       #single adaptive lasso run with ridge weighting and nu = 1
       adalasso.model <- cv.glmnet(X , Y , family = "gaussian" ,
                                   lambda = lambda.try , 
                                   penalty.factor = 1 / abs(best.ridge.coefs)^nu.try[i])
       lambda.adalasso.opt <- adalasso.model$lambda.min
       best.adalasso.coefs <- predict(adalasso.model , type = "coefficients" ,
                                      s = lambda.adalasso.opt)[-1]
       adalasso.nu.cv[[i]] <- list(model = list(full.model = adalasso.model , 
                                                lambda = lambda.adalasso.opt , 
                                                coefs = best.adalasso.coefs) , 
                                   metrics_and_info = list(model.seed = seed , 
                                                           ridge.coefs = best.ridge.coefs ,
                                                           weights = 1 / abs(best.ridge.coefs)^nu.try[i] , 
                                                           nu = nu.try[i] , 
                                                           lambda = lambda.adalasso.opt , 
                                                           coefs = best.adalasso.coefs , 
                                                           mpe = adalasso.model$cvm[which(adalasso.model$lambda == lambda.adalasso.opt)] , 
                                                           mpe.sd = adalasso.model$cvsd[which(adalasso.model$lambda == lambda.adalasso.opt)] , 
                                                           fpr = length(which(best.adalasso.coefs[c(5:p)] != 0)) / length(best.adalasso.coefs[c(5:p)]) , 
                                                           fnr = length(which(best.adalasso.coefs[c(1:4)] == 0)) / length(best.adalasso.coefs[1:4])))
}
#find minimizing nu/gamma
adalasso.nu.cv.mpe <- numeric()
for(i in 1:length(adalasso.nu.cv)) {
       adalasso.nu.cv.mpe[i] <- adalasso.nu.cv[[i]]$metrics_and_info$mpe
}
which.min(adalasso.nu.cv.mpe)
#store BEST adalasso result
adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]








