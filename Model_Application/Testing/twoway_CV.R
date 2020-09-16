#load libraries
library(quantreg)

#lsa.linear.txt
ladlasso.rq <- function(x , y , adalass) {
       n <- length(y)
       grid <- seq(log(0.01) , log(1400) , length.out = 100)
       grid <- exp(grid)
       rqob <- rq(y ~ 0 + x)
       BIC <- rep(0 , 100)
       weights <- 1 / abs(rqob$coef)
       
       for (k in 1:100){
              rqfit <- rq.fit.lasso(x , y , lambda = grid[k] * weights)
              betalad_tmp <- rqfit$coef
              betalad_tmp <- betalad_tmp * (betalad_tmp > 1e-8)
              mse <- mean(abs(rqfit$resi))
              mdsize <- length(which(betalad_tmp != 0))
              BIC[k] <- log(mse) + mdsize * log(n) / n
       }
       
       step <- which.min(BIC) 
       betalad <- rq.fit.lasso(x , y , lambda = grid[step] * weights)$coef
       ladlasso <- betalad * (betalad > 1e-8)
       
       #testing ladlasso mse
       coeff.lad <- c(adalass$intercept , ladlasso)
       coeff2.lad <- ladlasso              # get rid of intercept
       pred.lad <- x %*% coeff2.lad + coeff.lad[1]
       st.lad <- sum(coeff2.lad != 0)                                          # number nonzero
       mse.lad <- sum((y - pred.lad) ^ 2) / (n - st.lad - 1)
       return(list(fit = pred.lad , st = st.lad , mse = mse.lad , 
                   coeff = coeff2.lad , intercept = coeff.lad[1] , weights = weights))
}
ladlasso.ridge <- function(x , y , adalass) {
       n <- length(y)
       grid <- seq(log(0.01) , log(1400) , length.out = 100)
       grid <- exp(grid)
       rqob <- rq(y ~ 0 + x)
       BIC <- rep(0 , 100)
       weights <- 1 / abs(rqob$coef)
       
       for (k in 1:100){
              rqfit <- rq.fit.lasso(x , y , lambda = grid[k] * weights)
              betalad_tmp <- rqfit$coef
              betalad_tmp <- betalad_tmp * (betalad_tmp > 1e-8)
              mse <- mean(abs(rqfit$resi))
              mdsize <- length(which(betalad_tmp != 0))
              BIC[k] <- log(mse) + mdsize * log(n) / n
       }
       
       step <- which.min(BIC) 
       betalad <- rq.fit.lasso(x , y , lambda = grid[step] * weights)$coef
       ladlasso <- betalad * (betalad > 1e-8)
       
       #testing ladlasso mse
       coeff.lad <- c(adalass$intercept , ladlasso)
       coeff2.lad <- ladlasso              # get rid of intercept
       pred.lad <- x %*% coeff2.lad + coeff.lad[1]
       st.lad <- sum(coeff2.lad != 0)                                          # number nonzero
       mse.lad <- sum((y - pred.lad) ^ 2) / (n - st.lad - 1)
       return(list(fit = pred.lad , st = st.lad , mse = mse.lad , 
                   coeff = coeff2.lad , intercept = coeff.lad[1] , weights = weights))
}

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
       seed <- 
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
                                   metrics_and_info = list(weights = 1 / abs(best.ridge.coefs)^nu.try[i] , 
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








