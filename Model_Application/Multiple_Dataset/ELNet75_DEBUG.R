#load libraries
library(quantreg)
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.half <- readRDS()
#full.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/fulldata_091620.RData")
#half.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/500_data_10052020.RData")
#testing10.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/testing10_data_091720.RData")
#single.data <- testing10.data[[1]]
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")



#adaptive lasso function with two-way CV for selecting both lambda and nu/gamma
elnet75.sim.fnct <- function(data) { 
       #create simulation tracker
       tracker <- as.vector(unlist(data$conditions)) 
       #print tracker of status
       cat("n = " , tracker[1] , " , p = " , tracker[2] ,
           " , eta.x = " , tracker[3] , " , eta.y = " , tracker[4] ,
           " , g = " , tracker[5] , " , h = " , tracker[6] ,
           ";\n")
       #load X, Y, and p
       X <- data$X
       Y <- data$Y
       p <- data$conditions$p
       
       #grid of lambda values for cv
       lambda.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       
       #run cv.glmnet model
       elnet75.model <- cv.glmnet(X , Y , family = "gaussian" ,
                                    lambda = lambda.try , alpha = 0.75)
       lambda.elnet75.opt <- elnet75.model$lambda.min
       elnet75.coefs <- predict(elnet75.model , type = "coefficients" ,
                                       s = lambda.elnet75.opt)[-1]
      
       #return(elnet75.cv[[which.min(elnet75.cv.mpe)]])
       ###below is used to check that seeds are regenerated properly and not uniform
       return(list(model = list(full.model = elnet75.model , 
                                lambda = lambda.elnet75.opt , 
                                coefs = elnet75.coefs) , 
                   metrics = list(mpe = elnet75.model$cvm[which(elnet75.model$lambda == lambda.elnet75.opt)] , 
                                  mpe.sd = elnet75.model$cvsd[which(elnet75.model$lambda == lambda.elnet75.opt)] , 
                                  fpr = length(which(elnet75.coefs[c(5:p)] != 0)) / length(elnet75.coefs[c(5:p)]) , 
                                  fnr = length(which(elnet75.coefs[c(1:4)] == 0)) / length(elnet75.coefs[1:4])) ,
                   important = list(coefs = elnet75.coefs , 
                                    info = data.frame(cbind(n = tracker[1] , 
                                                            p = tracker[2] , 
                                                            eta.x = tracker[3] , 
                                                            eta.y = tracker[4] , 
                                                            g = tracker[5] , 
                                                            h = tracker[6] , 
                                                            data.seed = tracker[7] ,
                                                            alpha = 0.75 , 
                                                            lambda = lambda.elnet75.opt , 
                                                            mpe = elnet75.model$cvm[which(elnet75.model$lambda == lambda.elnet75.opt)] , 
                                                            mpe.sd = elnet75.model$cvsd[which(elnet75.model$lambda == lambda.elnet75.opt)] , 
                                                            fpr = length(which(elnet75.coefs[c(5:p)] != 0)) / length(elnet75.coefs[c(5:p)]) , 
                                                            fnr = length(which(elnet75.coefs[c(1:4)] == 0)) / length(elnet75.coefs[1:4])
                                                            )
                                                      )
                                    )
                   )
              )
}


#single test
#elnet75.single.test <- elnet75.sim.fnct(testing10.data[[1]])


#run across debug dataset
elnet75.debug <- debug.data %>%   
       map(safely(elnet75.sim.fnct))

saveRDS(elnet75.debug , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/elnet75_DEBUG.RData")


