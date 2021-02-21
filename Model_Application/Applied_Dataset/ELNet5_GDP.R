a#load libraries
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.full <- readRDS()
GDP <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/gdp_split.RData")


#elnet application function
elnet.sim.fnct <- function(data) {
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
       
       #set grid of lambda values
       lambda.elnet.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       #elnet model
       elnet.model <- cv.glmnet(X , Y , family = "gaussian" ,
                                lambda = lambda.elnet.try , alpha = 0.5)
       lambda.elnet.opt <- elnet.model$lambda.min
       elnet.coefs <- predict(elnet.model , type = "coefficients" ,
                              s = lambda.elnet.opt)[-1]
       n.coefs <- sum(elnet.coefs != 0)
       
       #specify test data
       test.X <- as.matrix(data$test[ , -c(1 , 15)])
       #cat("class(test.X) = " , class(test.X) , "\n")
       test.Y <- data$test$y.net
       
       #apply to test set
       pred.y <- test.X %*% elnet.coefs
       #cat("pred.y = " , pred.y , "\n")
       resid <- pred.y - test.Y
       resid.sq <- resid^2
       sum.resid.sq <- sum(resid.sq)
       mse <- sum.resid.sq / n

       #initialize important info dataframe
       #put conditions, model info, and metrics into list
       return(list(model = list(full.model = elnet.model , 
                                lambda = lambda.elnet.opt , 
                                coefs = elnet.coefs) , 
                   metrics = list(mse = mse , 
                                   n.coefs = n.coefs
                                  )
                   )
       )

}




#run across full dataset
elnet.full <- GDP %>%   
       map(safely(elnet.sim.fnct))

saveRDS(elnet.full , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Applied_Storage/elnet_GDP.RData")


