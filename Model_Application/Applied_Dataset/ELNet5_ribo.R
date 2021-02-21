a#load libraries
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.full <- readRDS()
ribo <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/ribo_split.RData")


#elnet5 application function
elnet5.sim.fnct <- function(data) {
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
       lambda.elnet5.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       #elnet5 model
       elnet5.model <- cv.glmnet(X , Y , family = "gaussian" ,
                                lambda = lambda.elnet5.try , alpha = 0.5)
       lambda.elnet5.opt <- elnet5.model$lambda.min
       elnet5.coefs <- predict(elnet5.model , type = "coefficients" ,
                              s = lambda.elnet5.opt)[-1]
       n.coefs <- sum(elnet5.coefs != 0)
       
       #specify test data
       test.X <- as.matrix(data$test[ , -1])
       #cat("class(test.X) = " , class(test.X) , "\n")
       test.Y <- data$test[ , 1]
       
       #apply to test set
       pred.y <- test.X %*% elnet5.coefs
       #cat("pred.y = " , pred.y , "\n")
       resid <- pred.y - test.Y
       resid.sq <- resid^2
       sum.resid.sq <- sum(resid.sq)
       mse <- sum.resid.sq / n

       #initialize important info dataframe
       #put conditions, model info, and metrics into list
       return(list(model = list(full.model = elnet5.model , 
                                lambda = lambda.elnet5.opt , 
                                coefs = elnet5.coefs) , 
                   metrics = list(mse = mse , 
                                   n.coefs = n.coefs
                                  )
                   )
       )

}




#run across full dataset
elnet5.full <- ribo %>%   
       map(safely(elnet5.sim.fnct))

saveRDS(elnet5.full , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Applied_Storage/elnet5_ribo.RData")


