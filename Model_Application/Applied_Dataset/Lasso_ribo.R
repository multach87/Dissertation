a#load libraries
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.full <- readRDS()
ribo <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/ribo_split.RData")


#lasso application function
lasso.sim.fnct <- function(data) {
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
       
       #set grid of lambda values
       lambda.lasso.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       #lasso model
       lasso.model <- cv.glmnet(X , Y , family = "gaussian" ,
                                lambda = lambda.lasso.try)
       lambda.lasso.opt <- lasso.model$lambda.min
       lasso.coefs <- predict(lasso.model , type = "coefficients" ,
                              s = lambda.lasso.opt)[-1]
       n.coefs <- sum(lasso.coefs != 0)
       
       #specify test data
       test.X <- as.matrix(data$test[ , -1])
       #cat("class(test.X) = " , class(test.X) , "\n")
       test.Y <- data$test[ , 1]
       
       #apply to test set
       pred.y <- test.X %*% lasso.coefs
       #cat("pred.y = " , pred.y , "\n")
       resid <- pred.y - test.Y
       resid.sq <- resid^2
       sum.resid.sq <- sum(resid.sq)
       mse <- sum.resid.sq / n

       #initialize important info dataframe
       #put conditions, model info, and metrics into list
       return(list(model = list(full.model = lasso.model , 
                                lambda = lambda.lasso.opt , 
                                coefs = lasso.coefs) , 
                   metrics = list(mse = mse , 
                                   n.coefs = n.coefs
                                  )
                   )
       )

}




#run across full dataset
lasso.full <- ribo %>%   
       map(safely(lasso.sim.fnct))

saveRDS(lasso.full , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Applied_Storage/lasso_ribo.RData")


