#load packages
library(hqreg)
library(glmnet)

#load data
#load data
DEBUG.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/debug_data_091620.RData")

#set objects for standard model run
X <- DEBUG.data[[1]]$X
Y <- DEBUG.data[[1]]$Y
n <- length(Y)
p <- DEBUG.data[[1]]$conditions$p
#set possible lambda values
lambda.lasso.try <- seq(log(1400) , log(0.01) , length.out = 100)
lambda.lasso.try <- exp(lambda.lasso.try)

#cv model application: non-adaptive penalty
test <- cv.hqreg(X = X , y = Y , method = "huber" , gamma = 1.345 , 
                 lambda = lambda.lasso.try , alpha = 0.5 , preprocess = "standardize" , 
                 screen = "SR" , FUN = "hqreg" , type.measure = "mse")
which.min(test$cve)
test$cve[85]
test$lambda[85]
test$fit$beta[-1 , 85]

#cv model application: adaptive penalty
##grid of nu/gamma values to try
nu.try <- exp(seq(log(0.01) , log(10) , length.out = 100))
##ridge setup for lambda weighting/penalty
ridge.model <- cv.glmnet(x = X , y = Y , lambda = lambda.lasso.try , alpha = 0)
lambda.ridge.opt <- ridge.model$lambda.min
best.ridge.coefs <- predict(ridge.model , type = "coefficients" ,
                            s = lambda.ridge.opt)

##actual model: one nu value
test3 <- cv.hqreg(X = X , y = Y , method = "huber" , gamma = 1.345 , 
                     lambda = lambda.lasso.try , alpha = 0.5 , preprocess = "standardize" , 
                     screen = "SR" , penalty.factor = 1 / abs(best.ridge.coefs)^nu.try[1] , 
                     FUN = "hqreg" , type.measure = "mse")
which.min(test3$cve)
test3$cve[78]
test3$lambda[78]
test3$fit$beta[-1 , 85]
##for all nu values
##initialize list of all values
test2 <- list()
##initialize matrices of metrics and minimizing results
huberelnet5.nu.cv.lambda <- numeric()
huberelnet5.nu.cv.mse <- numeric()
huberelnet5.nu.cv.coefs <- list()
#loop model over nu values
for(i in 1:length(nu.try)) {
  test2[[i]] <- cv.hqreg(X = X , y = Y , method = "huber" , gamma = 1.345 , 
                       lambda = lambda.lasso.try , alpha = 0.5 , preprocess = "standardize" , 
                       screen = "SR" , penalty.factor = 1 / abs(best.ridge.coefs)^nu.try[i] , 
                       FUN = "hqreg" , type.measure = "mse")
  huberelnet5.nu.cv.mse[i] <- min(test2[[i]]$cve)
  huberelnet5.nu.cv.lambda[i] <- test2[[i]]$lambda.min
  huberelnet5.nu.cv.coefs[[i]] <- test2[[i]]$fit$beta[-1 , which.min(test2[[i]]$cve)]
}

min(huberelnet5.nu.cv.mse)
which.min(huberelnet5.nu.cv.mse)
nu.try[which.min(huberelnet5.nu.cv.mse)]
huberelnet5.nu.cv.coefs[[which.min(huberelnet5.nu.cv.mse)]]
