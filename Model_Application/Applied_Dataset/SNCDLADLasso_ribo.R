#load libraries
library(glmnet)
library(magrittr)
library(purrr)
library(hqreg)

#load data
#data.full <- readRDS()
ribo <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/ribo_split.RData")

#lasso application function
ladlasso.sim.fnct <- function(data) {
        #create simulation tracker
        #tracker <- as.vector(unlist(data$conditions)) 
        
        #print tracker of status
        cat("iteration = " , data$track , ";\n")
      
       #load X, Y, p, n
       X <- as.matrix(data$train[ , -1])
       #cat("X = " , X , "\n")
       #cat("class(X) = " , class(X) , "\n")
       Y <- data$train[ , 1]
       #cat("class(Y) = " , class(Y) , "\n")
       n <- length(Y)
       
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
       ##initialize full list of LAD lasso results from each nu/gamma
       LADlasso.nu.cv.full <- list()
       ##initialize matrices of metrics and minimizing results
       LADlasso.nu.cv.lambda <- numeric()
       LADlasso.nu.cv.mse <- numeric()
       LADlasso.nu.cv.msesd <- numeric()
       LADlasso.nu.cv.coefs <- list()
       ##Loop over nu/gamma values for CV, storing minimizing lambda within each nu/gamma
       for(i in 1:length(nu.try)) {
         invisible(capture.output(LADlasso.nu.cv.full[[i]] <- cv.hqreg(X = X , y = Y , method = "quantile" , tau = 0.5 , 
                                                                       lambda = lambda.try , alpha = 1.0 , preprocess = "standardize" , 
                                                                       screen = "ASR" , penalty.factor = 1 / abs(best.ridge.coefs)^nu.try[i] , 
                                                                       FUN = "hqreg" , type.measure = "mse")))
         LADlasso.nu.cv.mse[i] <- min(LADlasso.nu.cv.full[[i]]$cve)
         LADlasso.nu.cv.msesd[i] <- LADlasso.nu.cv.full[[i]]$cvse[which.min(LADlasso.nu.cv.full[[i]]$cve)]
         LADlasso.nu.cv.lambda[i] <- LADlasso.nu.cv.full[[i]]$lambda.min
         LADlasso.nu.cv.coefs[[i]] <- LADlasso.nu.cv.full[[i]]$fit$beta[-1 , which.min(LADlasso.nu.cv.full[[i]]$cve)]
       }
       
       #specify minimizing nu value and resulting model info
       nu.opt <- nu.try[which.min(LADlasso.nu.cv.mse)]
       lambda.opt <- LADlasso.nu.cv.lambda[which.min(LADlasso.nu.cv.mse)]
       weights.opt <- 1 / abs(best.ridge.coefs)^nu.opt
       ladlasso.coefs <- LADlasso.nu.cv.coefs[[which.min(LADlasso.nu.cv.mse)]]
       LADlasso.mse.min <- min(LADlasso.nu.cv.mse)
       LADlasso.mse.min.se <- LADlasso.nu.cv.msesd[which.min(LADlasso.nu.cv.mse)]
       #cat("post.predict.2 \n")
       n.coefs <- sum(ladlasso.coefs != 0)
       
       #specify test data
       test.X <- as.matrix(data$test[ , -1])
       #cat("class(test.X) = " , class(test.X) , "\n")
       test.Y <- data$test[ , 1]
       
       #apply to test set
       pred.y <- test.X %*% ladlasso.coefs
       #cat("pred.y = " , pred.y , "\n")
       resid <- pred.y - test.Y
       resid.sq <- resid^2
       sum.resid.sq <- sum(resid.sq)
       mse <- sum.resid.sq / n

       #initialize important info dataframe
       #put conditions, model info, and metrics into list
       return(list(model = list(#full.model = ladlasso.model , 
                                lambda = lambda.opt , 
                                coefs = ladlasso.coefs) , 
                   metrics = list(mse = mse , 
                                   n.coefs = n.coefs
                                  )
                   )
       )

}




#run across full dataset
ladlasso.full <- ribo %>%   
       map(safely(ladlasso.sim.fnct))

saveRDS(ladlasso.full , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Applied_Storage/SNCDladlasso_ribo.RData")


