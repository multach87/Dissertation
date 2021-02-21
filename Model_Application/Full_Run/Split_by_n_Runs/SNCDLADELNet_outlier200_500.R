#load packages
library(hqreg)
library(glmnet)
library(magrittr)
library(purrr)

#load data
HALF.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/outlier200.RData")

SNCDLAD.elnet5.sim.fnct <- function(data) { 
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
  ##initialize full list of LAD elnet5 results from each nu/gamma
  LADelnet5.nu.cv.full <- list()
  ##initialize matrices of metrics and minimizing results
  LADelnet5.nu.cv.lambda <- numeric()
  LADelnet5.nu.cv.mse <- numeric()
  LADelnet5.nu.cv.msesd <- numeric()
  LADelnet5.nu.cv.coefs <- list()
  ##Loop over nu/gamma values for CV, storing minimizing lambda within each nu/gamma
  for(i in 1:length(nu.try)) {
    invisible(capture.output(LADelnet5.nu.cv.full[[i]] <- cv.hqreg(X = X , y = Y , method = "quantile" , tau = 0.5 , 
                                                                  lambda = lambda.try , alpha = 0.5 , preprocess = "standardize" , 
                                                                  screen = "SR" , penalty.factor = 1 / abs(best.ridge.coefs)^nu.try[i] , 
                                                                  FUN = "hqreg" , type.measure = "mse")))
    LADelnet5.nu.cv.mse[i] <- min(LADelnet5.nu.cv.full[[i]]$cve)
    LADelnet5.nu.cv.msesd[i] <- LADelnet5.nu.cv.full[[i]]$cvse[which.min(LADelnet5.nu.cv.full[[i]]$cve)]
    LADelnet5.nu.cv.lambda[i] <- LADelnet5.nu.cv.full[[i]]$lambda.min
    LADelnet5.nu.cv.coefs[[i]] <- LADelnet5.nu.cv.full[[i]]$fit$beta[-1 , which.min(LADelnet5.nu.cv.full[[i]]$cve)]
  }
  
  #specify minimizing nu value and resulting model info
  nu.opt <- nu.try[which.min(LADelnet5.nu.cv.mse)]
  lambda.opt <- LADelnet5.nu.cv.lambda[which.min(LADelnet5.nu.cv.mse)]
  weights.opt <- 1 / abs(best.ridge.coefs)^nu.opt
  coefs.opt <- LADelnet5.nu.cv.coefs[[which.min(LADelnet5.nu.cv.mse)]]
  LADelnet5.mse.min <- min(LADelnet5.nu.cv.mse)
  LADelnet5.mse.min.se <- LADelnet5.nu.cv.msesd[which.min(LADelnet5.nu.cv.mse)]
  
  #return(adaelnet55.nu.cv[[which.min(adaelnet55.nu.cv.mpe)]])
  #store BEST adaelnet55 result plus all seeds
  ###below is used to check that seeds are regenerated properly and not uniform
  return(list(full = list(
                 ridge.coefs = best.ridge.coefs ,
                 weights.opt = weights.opt , 
                 coefs.opt = coefs.opt) ,
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
                                 mpe = LADelnet5.mse.min ,
                                 mpe.sd = LADelnet5.mse.min.se ,
                                 fpr = length(which(coefs.opt[c(5:p)] != 0)) / length(coefs.opt[c(5:p)]) , 
                                 fnr = length(which(coefs.opt[c(1:4)] == 0)) / length(coefs.opt[1:4])
    )
    )
  )
  )
}

#run across full dataset
LADelnet5.HALF <- HALF.data %>%   
  map(safely(SNCDLAD.elnet5.sim.fnct))

saveRDS(LADelnet5.HALF , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADelnet5_outlier200_500_COEFS.RData")
