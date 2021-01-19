#load packages
library(hqreg)
library(glmnet)

#load data
#load data
DEBUG.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/debug_data_091620.RData")

SNCDHuber.elnet5.sim.fnct <- function(data) { 
  #create simulation tracker
  tracker <- as.vector(unlist(data$conditions)) 
  #print tracker of status
  cat("n = " , tracker[1] , " , p = " , tracker[2] ,
      " , eta.x = " , tracker[3] , " , eta.y = " , tracker[4] ,
      " , g = " , tracker[5] , " , h = " , tracker[6] ,
      ";\n")
  #load X, Y, n, and p
  X <- data$X
  Y <- data$Y
  n <- length(Y)
  p <- data$conditions$p
  #lambdas to try for regularization
  lambda.try <- seq(log(1400) , log(0.01) , length.out = 100)
  lambda.try <- exp(lambda.try)
  
  #nu/gamma selection cv
  ##ridge coefs for weighting
  ridge.model <- cv.glmnet(x = X , y = Y , lambda = lambda.try , alpha = 0)
  lambda.ridge.opt <- ridge.model$lambda.min
  best.ridge.coefs <- predict(ridge.model , type = "coefficients" ,
                              s = lambda.ridge.opt)[-1]
  ##grid of nu/gamma values to try
  nu.try <- exp(seq(log(0.01) , log(10) , length.out = 100))
  ##initialize full list of huber elnet results from each nu/gamma
  huberelnet5.nu.cv.full <- list()
  ##initialize matrices of metrics and minimizing results
  huberelnet5.nu.cv.lambda <- numeric()
  huberelnet5.nu.cv.mse <- numeric()
  huberelnet5.nu.cv.msesd <- numeric()
  huberelnet5.nu.cv.coefs <- list()
  ##Loop over nu/gamma values for CV, storing minimizing lambda within each nu/gamma
  for(i in 1:length(nu.try)) {
    huberelnet5.nu.cv.full[[i]] <- cv.hqreg(X = X , y = Y , method = "huber" , gamma = 1.345 , 
                           lambda = lambda.try , alpha = 0.5 , preprocess = "standardize" , 
                           screen = "SR" , penalty.factor = 1 / abs(best.ridge.coefs)^nu.try[i] , 
                           FUN = "hqreg" , type.measure = "mse")
    huberelnet5.nu.cv.mse[i] <- min(huberelnet5.nu.cv.full[[i]]$cve)
    huberelnet5.nu.cv.msesd[i] <- huberelnet5.nu.cv.full[[i]]$cvse[which.min(huberelnet5.nu.cv.full[[i]]$cve)]
    huberelnet5.nu.cv.lambda[i] <- huberelnet5.nu.cv.full[[i]]$lambda.min
    huberelnet5.nu.cv.coefs[[i]] <- huberelnet5.nu.cv.full[[i]]$fit$beta[-1 , which.min(huberelnet5.nu.cv.full[[i]]$cve)]
  }
  
  #specify minimizing nu value and resulting model info
  nu.opt <- nu.try[which.min(huberelnet5.nu.cv.mse)]
  lambda.opt <- huberelnet5.nu.cv.lambda[which.min(huberelnet5.nu.cv.mse)]
  weights.opt <- 1 / abs(best.ridge.coefs)^nu.opt
  coefs.opt <- huberelnet5.nu.cv.coefs[[which.min(huberelnet5.nu.cv.mse)]]
  huberelnet5.mse.min <- min(huberelnet5.nu.cv.mse)
  huberelnet5.mse.min.se <- huberelnet5.nu.cv.msesd[which.min(huberelnet5.nu.cv.mse)]
  
  #return(adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]])
  #store BEST adaelnet5 result plus all seeds
  ###below is used to check that seeds are regenerated properly and not uniform
  return(list(full = list(everything = huberelnet5.nu.cv.full , 
                          nu.cv.lambda = huberelnet5.nu.cv.lambda ,
                          nu.cv.mse = huberelnet5.nu.cv.mse ,
                          nu.cv.msesd = huberelnet5.nu.cv.msesd , 
                          nu.cv.coefs = huberelnet5.nu.cv.coefs) ,
              important = data.frame(cbind(n = tracker[1] ,
                                           p = tracker[2] ,
                                           eta.x = tracker[3] ,
                                           eta.y = tracker[4] ,
                                           g = tracker[5] ,
                                           h = tracker[6] ,
                                           data.seed = tracker[7] ,
                                           alpha = 0.5 ,
                                           lambda = lambda.opt ,
                                           nu = nu.opt ,
                                           mpe = huberelnet5.mse.min ,
                                           mpe.sd = huberelnet5.mse.min.se ,
                                           fpr = length(which(coefs.opt[c(5:p)] != 0)) / length(coefs.opt[c(5:p)]) , 
                                           fnr = length(which(coefs.opt[c(1:4)] == 0)) / length(coefs.opt[1:4])
                                             )
                                     )
              )
         )
}

test <- SNCDHuber.elnet5.sim.fnct(DEBUG.data[[1]])






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
test3$cvse[78]
test3$cvse[which.min(test3$cve)]
test3$lambda[78]
test3$fit$beta[-1 , 85]
##for all nu values
##initialize list of all values
test2 <- list()
##initialize matrices of metrics and minimizing results
huberelnet5.nu.cv.lambda <- numeric()
huberelnet5.nu.cv.mse <- numeric()
huberelnet5.nu.cv.coefs <- list()
##ridge setup for lambda weighting/penalty
ridge.model <- cv.glmnet(x = X , y = Y , lambda = lambda.lasso.try , alpha = 0)
lambda.ridge.opt <- ridge.model$lambda.min
best.ridge.coefs <- predict(ridge.model , type = "coefficients" ,
                            s = lambda.ridge.opt)[-1]
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
coefs.opt <- huberelnet5.nu.cv.coefs[[which.min(huberelnet5.nu.cv.mse)]]

#testing mpe vs mse
pred.huberelnet5 <- X %*% coefs.opt
#store number of nonzero coefs
st.huberelnet5 <- sum(coefs.opt != 0)                                          # number nonzero

#generate mpe and sd(mpe) for model
huberelnet5.mse.min <- sum((Y - pred.huberelnet5) ^ 2) / (n - st.huberelnet5 - 1)
huberelnet5.mse.min.sd <- sd((Y - pred.huberelnet5) ^ 2 / (n - st.huberelnet5 - 1))
