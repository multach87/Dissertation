a#load libraries
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.full <- readRDS()
GDP <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/gdp_split.RData")


#lasso application function
lasso.sim.fnct <- function(data) {
        #create simulation tracker
        #tracker <- as.vector(unlist(data$conditions)) 
        
        #print tracker of status
        cat("iteration = " , data$track , ";\n")
      
       #load X, Y, p, n
       X <- data$train[ , -c("y.net")]
       Y <- data$train$y.net
       n <- length(Y)
       
       #set grid of lambda values
       lambda.lasso.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       #lasso model
       lasso.model <- cv.glmnet(X , Y , family = "gaussian" ,
                                lambda = lambda.lasso.try)
       lambda.lasso.opt <- lasso.model$lambda.min
       lasso.coefs <- predict(lasso.model , type = "coefficients" ,
                              s = lambda.lasso.opt)[-1]
       
       #
       #initialize important info dataframe
       #put conditions, model info, and metrics into list
       return(list(seed.model = seed.model ,  
                   model = list(full.model = lasso.model , 
                                lambda = lambda.lasso.opt , coefs = lasso.coefs) , 
                   metrics = list(mpe = lasso.model$cvm[which(lasso.model$lambda == lambda.lasso.opt)] , 
                                   mpe.sd = lasso.model$cvsd[which(lasso.model$lambda == lambda.lasso.opt)] , 
                                   fpr = length(which(lasso.coefs[c(5:p)] != 0)) / length(lasso.coefs[c(5:p)]) , 
                                   fnr = length(which(lasso.coefs[c(1:4)] == 0)) / length(lasso.coefs[1:4])) , 
                   important = list(diagnostics = data.frame(cbind(data.seed = tracker[7] ,
                                                                   model.seed = seed.model)) , 
                                    coefs = lasso.coefs , 
                                    info = data.frame(cbind(n = tracker[1] , 
                                                            p = tracker[2] , 
                                                            eta.x = tracker[3] , 
                                                            eta.y = tracker[4] , 
                                                            g = tracker[5] , 
                                                            h = tracker[6] , 
                                                            data.seed = tracker[7] ,
                                                            model.seed = seed.model , 
                                                            lambda = lambda.lasso.opt , 
                                                            mpe = lasso.model$cvm[which(lasso.model$lambda == lambda.lasso.opt)] , 
                                                            mpe.sd = lasso.model$cvsd[which(lasso.model$lambda == lambda.lasso.opt)] , 
                                                            fpr = length(which(lasso.coefs[c(5:p)] != 0)) / length(lasso.coefs[c(5:p)]) , 
                                                            fnr = length(which(lasso.coefs[c(1:4)] == 0)) / length(lasso.coefs[1:4])
                                                            )
                                                      )
                                    )
                                    )
                   )                                 

}




#run across full dataset
lasso.full <- GDP %>%   
       map(safely(lasso.sim.fnct))

saveRDS(lasso.full , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Applied_Storage/lasso_GDP.RData")


