#libraries
library(glmnet)
library(purrr)
library(magrittr)
library(pense)

#load data
#DEBUG.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/debug_data_091620.RData")
#HD190.data <- HD.data[1:500]
#rm(list = c("HD.data"))
HD.data_DEBUG <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")
HD_DEBUGshort.data <- HD.data_DEBUG[c(1 , 13 , 22 , 34 , 43 , 54 , 64 , 76 , 85 , 97 , 
                                      106 , 118 , 127 , 139 , 148 , 160)]

#NOTE: "cv" is still in a lot of the object names. This was to prevent potential human error
##in removing/replacing those names when moving from pense_cv() --> pense()
##Also, for ease of switching back to pense_cv() if desired
#function
pense5.sim.fnct <- function(data) {
  #create simulation tracker
  tracker <- as.vector(unlist(data$conditions)) 
  
  #print tracker of status
  cat("n = " , tracker[1] , " , p = " , tracker[2] ,
      " , eta.x = " , tracker[3] , " , eta.y = " , tracker[4] ,
      " , g = " , tracker[5] , " , h = " , tracker[6] ,
      ";\n")
  
  #load X, Y, n, p
  X <- data$X
  Y <- data$Y
  n <- length(Y)
  p <- data$conditions$p
  
  #set seed
  #seed.pensecv <- data$seeds[ , "seed.11"]
  
  #set possible lambda values
  lambda.lasso.try <- seq(log(0.01) , log(1400) , length.out = 100)
  lambda.lasso.try <- exp(lambda.lasso.try)
  
  #set.seed(seed.pensecv)
  pense.cv <- pense(x = X , y = Y , alpha = 0.5 , 
                    #cv_k = 5 , cv_repl = 10 , cv_metric = "rmspe" , 
                    lambda = lambda.lasso.try , 
                    intercept = FALSE)
  
  pense.lambda.cv <- list()
  pense.lambda <- numeric()
  pense.lambda.objfvalue <- numeric()
  pense.lambda.mpe <- numeric()
  pense.lambda.mpe.sd <- numeric()
  pense.lambda.coef <- list()
  
  for(k in 1:length(lambda.lasso.try)) {
    #fill information from each lambda used in pense models
    pense.lambda[k] <- pense.cv$estimates[[k]]$lambda
    pense.lambda.objfvalue[k] <- pense.cv$estimates[[k]]$objf_value
    pense.lambda.coef[[k]] <- pense.cv$estimates[[k]]$beta
    
    #generate y-hats for each observation
    pred.pense <- X %*% pense.lambda.coef[[k]] #+ coeff.lad[1]
    
    #store number of nonzero coefs
    st.pense <- sum(pense.lambda.coef[[k]] != 0)                                          # number nonzero
    
    #generate mpe and sd(mpe) for model
    pense.lambda.mpe[k] <- sum((Y - pred.pense) ^ 2) / (n - st.pense - 1)
    pense.lambda.mpe.sd[k] <- sd((Y - pred.pense) ^ 2 / (n - st.pense - 1))
    
    #save list of all info from best model
    
    #
  }
  
  pense.coefs.final <- pense.lambda.coef[[which.min(pense.lambda.mpe)]]
  pense.lambda.final <- pense.lambda[[which.min(pense.lambda.mpe)]]
  pense.mpe.final <- min(pense.lambda.mpe)
  pense.mpe.sd.final <- pense.lambda.mpe.sd[[which.min(pense.lambda.mpe)]]
  pense.pred.final <- X %*% pense.coefs.final

  return(list(full = pense.cv , 
              mpes = pense.lambda.mpe ,
              pense.obj = pense.lambda.objfvalue ,
              model = list(coefficient = pense.coefs.final , 
                           fit = pense.pred.final , 
                           mpe = pense.mpe.final , 
                           mpe.sd = pense.mpe.sd.final ,
                           lambda.opt = pense.lambda.final) , 
              coefs = pense.coefs.final , 
              info = data.frame(cbind(n = tracker[1] , 
                                      p = tracker[2] , 
                                      eta.x = tracker[3] , 
                                      eta.y = tracker[4] , 
                                      g = tracker[5] , 
                                      h = tracker[6] , 
                                      data.seed = tracker[7] , 
                                      lambda.pense = pense.lambda.final , 
                                      alpha = 0.5 , 
                                      mpe = pense.mpe.final , 
                                      mpe.sd = pense.mpe.sd.final ,
                                      fpr = length(which(pense.coefs.final[c(5:p)] != 0)) / length(pense.coefs.final[c(5:p)]) , 
                                      fnr = length(which(pense.coefs.final[c(1:4)] == 0)) / length(pense.coefs.final[1:4]))
              )
              )
  )
  
  
}

#run across full dataset
pense5.HD.debug <- HD_DEBUGshort.data %>%   
  map(safely(pense5.sim.fnct))

saveRDS(pense5.HD.debug , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/pense5_HD_DEBUG.RData")

