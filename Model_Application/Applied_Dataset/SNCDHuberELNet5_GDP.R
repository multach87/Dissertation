#load libraries
library(glmnet)
library(magrittr)
library(purrr)
library(hqreg)

#load data
#data.full <- readRDS()
GDP <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/gdp_split.RData")

#lasso application function
huberelnet5.sim.fnct <- function(data) {
        #create simulation tracker
        #tracker <- as.vector(unlist(data$conditions)) 
        
        #print tracker of status
        cat("iteration = " , data$track , ";\n")
      
       #load X, Y, p, n
       X <- as.matrix(data$train[ , -c(1 , 15)])
       #cat("X = " , X , "\n")
       #cat("class(X) = " , class(X) , "\n")
       Y <- data$train$y.net
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
       huberelnet5.nu.cv.full <- list()
       ##initialize matrices of metrics and minimizing results
       huberelnet5.nu.cv.lambda <- numeric()
       huberelnet5.nu.cv.mse <- numeric()
       huberelnet5.nu.cv.msesd <- numeric()
       huberelnet5.nu.cv.coefs <- list()
       ##Loop over nu/gamma values for CV, storing minimizing lambda within each nu/gamma
       for(i in 1:length(nu.try)) {
         invisible(capture.output(huberelnet5.nu.cv.full[[i]] <- cv.hqreg(X = X , y = Y , method = "quantile" , gamma = 1.345 , 
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
       huberelnet5.coefs <- huberelnet5.nu.cv.coefs[[which.min(huberelnet5.nu.cv.mse)]]
       huberelnet5.mse.min <- min(huberelnet5.nu.cv.mse)
       huberelnet5.mse.min.se <- huberelnet5.nu.cv.msesd[which.min(huberelnet5.nu.cv.mse)]
       #cat("post.predict.2 \n")
       n.coefs <- sum(huberelnet5.coefs != 0)
       
       #specify test data
       test.X <- as.matrix(data$test[ , -c(1 , 15)])
       #cat("class(test.X) = " , class(test.X) , "\n")
       test.Y <- data$test$y.net
       
       #apply to test set
       pred.y <- test.X %*% huberelnet5.coefs
       #cat("pred.y = " , pred.y , "\n")
       resid <- pred.y - test.Y
       resid.sq <- resid^2
       sum.resid.sq <- sum(resid.sq)
       mse <- sum.resid.sq / n

       #initialize important info dataframe
       #put conditions, model info, and metrics into list
       return(list(model = list(#full.model = huberelnet5.model , 
                                lambda = lambda.opt , 
                                coefs = huberelnet5.coefs) , 
                   metrics = list(mse = mse , 
                                   n.coefs = n.coefs
                                  )
                   )
       )

}




#run across full dataset
huberelnet5.full <- GDP %>%   
       map(safely(huberelnet5.sim.fnct))

saveRDS(huberelnet5.full , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Applied_Storage/SNCDhuberelnet5_GDP.RData")


