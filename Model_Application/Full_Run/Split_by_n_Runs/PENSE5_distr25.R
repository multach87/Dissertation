#libraries
library(glmnet)
library(purrr)
library(magrittr)
library(pense)

#load data
#data.full <- readRDS()
#full.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/")
#half.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/500_data_10052020.RData")
distr25.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/distr25.RData")
#debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
#testing10.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/testing10_data_091720.RData")
#single.data <- testing10.data[[1]]

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
  seed.pensecv <- data$seeds[ , "seed.11"]
  
  #set possible lambda values
  lambda.lasso.try <- seq(log(0.01) , log(1400) , length.out = 100)
  lambda.lasso.try <- exp(lambda.lasso.try)
  
  set.seed(seed.pensecv)
  pense.cv <- pense_cv(x = X , y = Y , alpha = 0.5 , 
                       cv_k = 5 , cv_repl = 10 , cv_metric = "rmspe" , 
                       lambda = lambda.lasso.try , 
                       intercept = FALSE)
  #coefficients
  pense.coefs <- coef(pense.cv)[-1]
  
  #fit/pred values
  Y.fit<- X %*% pense.coefs
  
  #store number of nonzero coefs
  st.pense <- sum(pense.coefs != 0)                                         # number nonzero
  
  #generate MSE and sd(MSE) for model
  n <- nrow(X)
  mse.pense <- sum((Y - Y.fit) ^ 2) / (n - st.pense - 1)
  sd.mse.pense <- sd((Y - Y.fit) ^ 2 / (n - st.pense - 1))
  
  #store minimizing lambda
  lambda.pense.opt <- pense.cv$cvres$lambda[which.min(pense.cv$cvres$cvavg)]

  return(list(full = pense.cv , 
              model = list(coefficient = pense.coefs , 
                           fit = Y.fit , 
                           mpe = mse.pense , 
                           mpe.sd = sd.mse.pense ,
                           lambda.opt = lambda.pense.opt) , 
              important = list(diagnostics = data.frame(cbind(data.seed = tracker[7] , 
                                                              model.seed.pense = seed.pensecv)
              ) , 
              coefs = pense.coefs , 
              info = data.frame(cbind(n = tracker[1] , 
                                      p = tracker[2] , 
                                      eta.x = tracker[3] , 
                                      eta.y = tracker[4] , 
                                      g = tracker[5] , 
                                      h = tracker[6] , 
                                      data.seed = tracker[7] ,
                                      model.seed.pense = seed.pensecv , 
                                      lambda.pense = lambda.pense.opt , 
                                      alpha = 0.5 , 
                                      mpe = mse.pense , 
                                      mpe.sd = sd.mse.pense ,
                                      fpr = length(which(pense.coefs[c(5:p)] != 0)) / length(pense.coefs[c(5:p)]) , 
                                      fnr = length(which(pense.coefs[c(1:4)] == 0)) / length(pense.coefs[1:4]))
              )
              )
  )
  )
  
  
}

pense5.distr25 <- distr25.data %>%   
  map(safely(pense5.sim.fnct))

saveRDS(pense5.distr25 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/pense5_distr25.RData")
