#load libraries
library(glmnet)
library(magrittr)
library(purrr)
library(hqreg)

#load data
#data.full <- readRDS()
ribo <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/ribo_split.RData")

#lasso application function
huberlasso.sim.fnct <- function(data) {
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
       ##initialize full list of huber lasso results from each nu/gamma
       huberlasso.nu.cv.full <- list()
       ##initialize matrices of metrics and minimizing results
       huberlasso.nu.cv.lambda <- numeric()
       huberlasso.nu.cv.mse <- numeric()
       huberlasso.nu.cv.msesd <- numeric()
       huberlasso.nu.cv.coefs <- list()
       ##Loop over nu/gamma values for CV, storing minimizing lambda within each nu/gamma
       for(i in 1:length(nu.try)) {
         invisible(capture.output(huberlasso.nu.cv.full[[i]] <- cv.hqreg(X = X , y = Y , method = "quantile" , gamma = 1.345 , 
                                                                       lambda = lambda.try , alpha = 1.0 , preprocess = "standardize" , 
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
       huberlasso.coefs <- huberlasso.nu.cv.coefs[[which.min(huberlasso.nu.cv.mse)]]
       huberlasso.mse.min <- min(huberlasso.nu.cv.mse)
       huberlasso.mse.min.se <- huberlasso.nu.cv.msesd[which.min(huberlasso.nu.cv.mse)]
       #cat("post.predict.2 \n")
       n.coefs <- sum(huberlasso.coefs != 0)
       
       #specify test data
       test.X <- as.matrix(data$test[ , -1])
       #cat("class(test.X) = " , class(test.X) , "\n")
       test.Y <- data$test[ , 1]
       
       #apply to test set
       pred.y <- test.X %*% huberlasso.coefs
       #cat("pred.y = " , pred.y , "\n")
       resid <- pred.y - test.Y
       resid.sq <- resid^2
       sum.resid.sq <- sum(resid.sq)
       mse <- sum.resid.sq / n

       #initialize important info dataframe
       #put conditions, model info, and metrics into list
       return(list(model = list(#full.model = huberlasso.model , 
                                lambda = lambda.opt , 
                                coefs = huberlasso.coefs) , 
                   metrics = list(mse = mse , 
                                   n.coefs = n.coefs
                                  )
                   )
       )

}




#run across full dataset
huberlasso.full <- ribo[1:2] %>%   
       map(safely(huberlasso.sim.fnct))

saveRDS(huberlasso.full , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Applied_Storage/SNCDhuberlasso_ribo.RData")


