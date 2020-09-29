#libraries
library(glmnet)
library(purrr)
library(magrittr)
library(pense)

#load data
#data.full <- readRDS()
#full.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/")
#debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
testing10.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/testing10_data_091720.RData")
single.data <- testing10.data[[1]]

#load X and Y
X <- single.data[["X"]]
Y <- single.data[["Y"]]

#function
elnet5.sim.funct <- function(data , alpha = 0.5) {
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
  seed.elnetcv <- data$seeds[ , "seed.11"]
  
  #set possible lambda values
  lambda.lasso.try <- seq(log(0.01) , log(1400) , length.out = 100)
  lambda.lasso.try <- exp(lambda.lasso.try)
  
  set.seed(seed.elnetcv)
  elnet.cv <- pense_cv(x = X , y = Y , alpha = alpha , 
                       cv_k = 5 , cv_repl = 10 , cv_metric = "rmspe" , 
                       lambda = lambda.lasso.try , 
                       intercept = FALSE)
  #coefficients
  elnet.coefs <- coef(elnet.cv)[-1]
  
  #fit/pred values
  Y.fit<- X %*% elnet.coefs
  
  #store number of nonzero coefs
  st.elnet <- sum(elnet.coefs != 0)                                         # number nonzero
  
  #generate MSE and sd(MSE) for model
  n <- nrow(X)
  mse.elnet <- sum((Y - Y.fit) ^ 2) / (n - st.lad - 1)
  sd.mse.elnet <- sd((Y - Y.fit) ^ 2 / (n - st.lad - 1))
  
  #store minimizing lambda
  lambda.elnet.opt <- elnet.cv$cvres$lambda[which.min(elnet.cv$cvres$cvavg)]
  
  #store alpha
  alpha <- alpha
  
  return(list(full = elnet.cv , 
              model = list(coefficient = elnet.coefs , 
                           fit = Y.fit , 
                           mpe = mse.elnet , 
                           mpe.sd = sd.mse.elnet ,
                           lambda.opt = lambda.elnet.opt) , 
              important = list(diagnostics = data.frame(cbind(data.seed = tracker[7] , 
                                                              model.seed.elnet = seed.elnetcv)
              ) , 
              coefs = elnet.coefs , 
              info = data.frame(cbind(n = tracker[1] , 
                                      p = tracker[2] , 
                                      eta.x = tracker[3] , 
                                      eta.y = tracker[4] , 
                                      g = tracker[5] , 
                                      h = tracker[6] , 
                                      data.seed = tracker[7] ,
                                      model.seed.elnet = seed.elnetcv , 
                                      lambda.elnet = lambda.elnet.opt , 
                                      alpha = alpha , 
                                      mpe = mse.elnet , 
                                      mpe.sd = sd.mse.elnet ,
                                      fpr = length(which(elnet.coefs[c(5:p)] != 0)) / length(elnet.coefs[c(5:p)]) , 
                                      fnr = length(which(elnet.coefs[c(1:4)] == 0)) / length(elnet.coefs[1:4]))
              )
              )
  )
  )
  
  
}
elnet75.sim.funct <- function(data , alpha = 0.75) {
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
  seed.elnetcv <- data$seeds[ , "seed.12"]
  
  #set possible lambda values
  lambda.lasso.try <- seq(log(0.01) , log(1400) , length.out = 100)
  lambda.lasso.try <- exp(lambda.lasso.try)
  
  set.seed(seed.elnetcv)
  elnet.cv <- pense_cv(x = X , y = Y , alpha = alpha , 
                       cv_k = 5 , cv_repl = 10 , cv_metric = "rmspe" , 
                       lambda = lambda.lasso.try , 
                       intercept = FALSE)
  #coefficients
  elnet.coefs <- coef(elnet.cv)[-1]
  
  #fit/pred values
  Y.fit<- X %*% elnet.coefs
  
  #store number of nonzero coefs
  st.elnet <- sum(elnet.coefs != 0)                                         # number nonzero
  
  #generate MSE and sd(MSE) for model
  n <- nrow(X)
  mse.elnet <- sum((Y - Y.fit) ^ 2) / (n - st.lad - 1)
  sd.mse.elnet <- sd((Y - Y.fit) ^ 2 / (n - st.lad - 1))
  
  #store minimizing lambda
  lambda.elnet.opt <- elnet.cv$cvres$lambda[which.min(elnet.cv$cvres$cvavg)]
  
  #store alpha
  alpha <- alpha
  
  return(list(full = elnet.cv , 
              model = list(coefficient = elnet.coefs , 
                           fit = Y.fit , 
                           mpe = mse.elnet , 
                           mpe.sd = sd.mse.elnet ,
                           lambda.opt = lambda.elnet.opt) , 
              important = list(diagnostics = data.frame(cbind(data.seed = tracker[7] , 
                                                              model.seed.elnet = seed.elnetcv)
              ) , 
              coefs = elnet.coefs , 
              info = data.frame(cbind(n = tracker[1] , 
                                      p = tracker[2] , 
                                      eta.x = tracker[3] , 
                                      eta.y = tracker[4] , 
                                      g = tracker[5] , 
                                      h = tracker[6] , 
                                      data.seed = tracker[7] ,
                                      model.seed.elnet = seed.elnetcv , 
                                      lambda.elnet = lambda.elnet.opt , 
                                      alpha = alpha , 
                                      mpe = mse.elnet , 
                                      mpe.sd = sd.mse.elnet ,
                                      fpr = length(which(elnet.coefs[c(5:p)] != 0)) / length(elnet.coefs[c(5:p)]) , 
                                      fnr = length(which(elnet.coefs[c(1:4)] == 0)) / length(elnet.coefs[1:4]))
              )
              )
  )
  )
  
  
}
elnet9.sim.funct <- function(data , alpha = 0.9) {
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
  seed.elnetcv <- data$seeds[ , "seed.13"]
  
  #set possible lambda values
  lambda.lasso.try <- seq(log(0.01) , log(1400) , length.out = 100)
  lambda.lasso.try <- exp(lambda.lasso.try)
  
  set.seed(seed.elnetcv)
  elnet.cv <- pense_cv(x = X , y = Y , alpha = alpha , 
                       cv_k = 5 , cv_repl = 10 , cv_metric = "rmspe" , 
                       lambda = lambda.lasso.try , 
                       intercept = FALSE)
  #coefficients
  elnet.coefs <- coef(elnet.cv)[-1]
  
  #fit/pred values
  Y.fit<- X %*% elnet.coefs
  
  #store number of nonzero coefs
  st.elnet <- sum(elnet.coefs != 0)                                         # number nonzero
  
  #generate MSE and sd(MSE) for model
  n <- nrow(X)
  mse.elnet <- sum((Y - Y.fit) ^ 2) / (n - st.lad - 1)
  sd.mse.elnet <- sd((Y - Y.fit) ^ 2 / (n - st.lad - 1))
  
  #store minimizing lambda
  lambda.elnet.opt <- elnet.cv$cvres$lambda[which.min(elnet.cv$cvres$cvavg)]
  
  #store alpha
  alpha <- alpha
  
  return(list(full = elnet.cv , 
              model = list(coefficient = elnet.coefs , 
                           fit = Y.fit , 
                           mpe = mse.elnet , 
                           mpe.sd = sd.mse.elnet ,
                           lambda.opt = lambda.elnet.opt) , 
              important = list(diagnostics = data.frame(cbind(data.seed = tracker[7] , 
                                                              model.seed.elnet = seed.elnetcv)
              ) , 
              coefs = elnet.coefs , 
              info = data.frame(cbind(n = tracker[1] , 
                                      p = tracker[2] , 
                                      eta.x = tracker[3] , 
                                      eta.y = tracker[4] , 
                                      g = tracker[5] , 
                                      h = tracker[6] , 
                                      data.seed = tracker[7] ,
                                      model.seed.elnet = seed.elnetcv , 
                                      lambda.elnet = lambda.elnet.opt , 
                                      alpha = alpha , 
                                      mpe = mse.elnet , 
                                      mpe.sd = sd.mse.elnet ,
                                      fpr = length(which(elnet.coefs[c(5:p)] != 0)) / length(elnet.coefs[c(5:p)]) , 
                                      fnr = length(which(elnet.coefs[c(1:4)] == 0)) / length(elnet.coefs[1:4]))
              )
              )
  )
  )
  
  
}

#test single function
test.5 <- elnet5.sim.funct(single.data)
test.75 <- elnet75.sim.funct(single.data)
test.9 <- elnet9.sim.funct(single.data)









#test model
set.seed(501)
lambda.lasso.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
#find minimizing results/use cv
set.seed(501)
elnet.cv <- pense_cv(x = X , y = Y , alpha = 0.5 , 
                     cv_k = 5 , cv_repl = 10 , cv_metric = "rmspe" , 
                     lambda = lambda.lasso.try , 
                     intercept = FALSE)


#fit/pred values
Y.fit<- X%*%coef(elnet.cv2)[-1]

#store number of nonzero coefs
st.lad <- sum(coef(elnet.cv2)[-1] != 0)                                         # number nonzero

#generate MSE and sd(MSE) for model
n <- nrow(X)
mse.lad <- sum((Y - Y.fit) ^ 2) / (n - st.lad - 1)
sd.mse.lad <- sd((Y - Y.fit) ^ 2 / (n - st.lad - 1))

#store lambda
lambda.lasso.opt <- elnet.cv2$cvres$lambda[which.min(elnet.cv2$cvres$cvavg)]








elnet.test <- pense(x = X , y = Y , alpha = 0.5 , 
                    lambda = lambda.lasso.try , intercept = FALSE)
#summary(elnet.test) NOT USEFUL STUFF
elnet.test$estimates[[100]]$beta










#find minimizing results by finding min objf_value - happens to be in order of lambda.lasso.try
objf_values <- numeric()
for(i in 1:length(elnet.test$estimates)) {
  objf_values[i] <- elnet.test$estimates[[i]]$objf_value
}


##try rerunning with randomized lambda.lasso.try
lambda.lasso.try.random <- sample(lambda.lasso.try , size = length(lambda.lasso.try) , replace = FALSE)
elnet.random.test <- pense(x = X , y = Y , alpha = 0.75 , 
                           lambda = lambda.lasso.try.random , 
                           intercept = FALSE)
objf_values.random <- numeric()
for(i in 1:length(elnet.random.test$estimates)) {
  objf_values.random[i] <- elnet.random.test$estimates[[i]]$objf_value
}
which.min(objf_values.random)



#test adaptive elnet model
set.seed(501)
lambda.lasso.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
adaelnet.test <- adapense_cv(x = X , y = Y , alpha = 0.5 , 
                    cv_k = 5 , cv_repl = 10 , 
                    lambda = lambda.lasso.try)
summary(adaelnet.test)
