#load packages
library(hqreg)
library(glmnet)
library(magrittr)
library(purrr)

#load data
HD.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HDSparsedata_112320.RData")

SNCDHuber.lasso.sim.fnct <- function(data) { 
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
  huberlasso.nu.cv.full <- list()
  ##initialize matrices of metrics and minimizing results
  huberlasso.nu.cv.lambda <- numeric()
  huberlasso.nu.cv.mse <- numeric()
  huberlasso.nu.cv.msesd <- numeric()
  huberlasso.nu.cv.coefs <- list()
  ##Loop over nu/gamma values for CV, storing minimizing lambda within each nu/gamma
  for(i in 1:length(nu.try)) {
    invisible(capture.output(huberlasso.nu.cv.full[[i]] <- cv.hqreg(X = X , y = Y , method = "huber" , gamma = 1.345 , 
                           lambda = lambda.try , alpha = 1 , preprocess = "standardize" , 
                           screen = "SR" , penalty.factor = 1 / abs(best.ridge.coefs)^nu.try[i] , 
                           FUN = "hqreg" , type.measure = "mse")))
    huberlasso.nu.cv.mse[i] <- min(huberlasso.nu.cv.full[[i]]$cve)
    huberlasso.nu.cv.msesd[i] <- huberlasso.nu.cv.full[[i]]$cvse[which.min(huberlasso.nu.cv.full[[i]]$cve)]
    huberlasso.nu.cv.lambda[i] <- huberlasso.nu.cv.full[[i]]$lambda.min
    huberlasso.nu.cv.coefs[[i]] <- huberlasso.nu.cv.full[[i]]$fit$beta[-1 , which.min(huberlasso.nu.cv.full[[i]]$cve)]
  }
  
  #specify minimizing nu value and resulting model info
  nu.opt <- nu.try[which.min(huberlasso.nu.cv.mse)]
  lambda.opt <- huberlasso.nu.cv.lambda[which.min(huberlasso.nu.cv.mse)]
  weights.opt <- 1 / abs(best.ridge.coefs)^nu.opt
  coefs.opt <- huberlasso.nu.cv.coefs[[which.min(huberlasso.nu.cv.mse)]]
  huberlasso.mse.min <- min(huberlasso.nu.cv.mse)
  huberlasso.mse.min.se <- huberlasso.nu.cv.msesd[which.min(huberlasso.nu.cv.mse)]
  
  #return(adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]])
  #store BEST adaelnet5 result plus all seeds
  ###below is used to check that seeds are regenerated properly and not uniform
  return(list(full = list(everything = huberlasso.nu.cv.full , 
                          nu.cv.lambda = huberlasso.nu.cv.lambda ,
                          nu.cv.mse = huberlasso.nu.cv.mse ,
                          nu.cv.msesd = huberlasso.nu.cv.msesd , 
                          nu.cv.coefs = huberlasso.nu.cv.coefs) ,
              important = data.frame(cbind(n = tracker[1] ,
                                           p = tracker[2] ,
                                           eta.x = tracker[3] ,
                                           eta.y = tracker[4] ,
                                           g = tracker[5] ,
                                           h = tracker[6] ,
                                           data.seed = tracker[7] ,
                                           alpha = 1 ,
                                           lambda = lambda.opt ,
                                           nu = nu.opt ,
                                           mpe = huberlasso.mse.min ,
                                           mpe.sd = huberlasso.mse.min.se ,
                                           fpr = length(which(coefs.opt[c(5:p)] != 0)) / length(coefs.opt[c(5:p)]) , 
                                           fnr = length(which(coefs.opt[c(1:4)] == 0)) / length(coefs.opt[1:4])
                                             )
                                     )
              )
         )
}

#run across full dataset
huberlasso.HD <- HD.data %>%   
  map(safely(SNCDHuber.lasso.sim.fnct))

saveRDS(huberlasso.HD , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/SNCDHuberLasso_HD_500.RData")
