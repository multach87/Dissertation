#load packages
library(hqreg)
library(glmnet)
library(magrittr)
library(purrr)

#load data
HALF.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/500_data_10052020.RData")

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
    invisible(capture.output(huberelnet5.nu.cv.full[[i]] <- cv.hqreg(X = X , y = Y , method = "huber" , gamma = 1.345 , 
                           lambda = lambda.try , alpha = 0.5 , preprocess = "standardize" , 
                           screen = "SR" , penalty.factor = 1 / abs(best.ridge.coefs)^nu.try[i] , 
                           FUN = "hqreg" , type.measure = "mse")))
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
  return(list(full = list(ridge.coefs = best.ridge.coefs ,
                          weights.opt = weights.opt , 
                          coefs.opt = as.numeric(coefs.opt)) ,
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

#run across full dataset
huberelnet5.HALF <- HALF.data %>%   
  map(safely(SNCDHuber.elnet5.sim.fnct))

saveRDS(huberelnet5.HALF , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/Huberelnet5_500_COEFS.RData")
