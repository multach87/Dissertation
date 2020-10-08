#load libraries
library(quantreg)
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.half <- readRDS()
#full.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/fulldata_091620.RData")
half.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/500_data_10052020.RData")
#testing10.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/testing10_data_091720.RData")
#single.data <- testing10.data[[1]]
#debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")



#adaptive lasso function with two-way CV for selecting both lambda and nu/gamma
adaelnet5.sim.fnct <- function(data) { 
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
       #seed.ridge <- data$seeds[ , "seed.14"]
       #set.seed(seed.ridge)
       #ridge coefs for weighting
       lambda.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       ridge.model <- cv.glmnet(x = X , y = Y , lambda = lambda.try , alpha = 0)
       lambda.ridge.opt <- ridge.model$lambda.min
       best.ridge.coefs <- predict(ridge.model , type = "coefficients" ,
                                   s = lambda.ridge.opt)[-1]
       ##grid of nu/gamma values to try
       nu.try <- exp(seq(log(0.01) , log(10) , length.out = 100))
       #seed.pre.nu <- data$seeds[ , "seed.15"]
       #set.seed(seed.pre.nu)
       #seed.nu <- sample(rnorm(n = 1000000000) , size = length(nu.try) , replace = FALSE)
       ##initialize list of best adaelnet5 results from each nu/gamma
       adaelnet5.nu.cv <- list()
       for(i in 1:length(nu.try)) {
              #seed <- seed.nu[i]
              #set.seed(seed)
              #single adaptive lasso run with ridge weighting and nu = 1
              adaelnet5.model <- cv.glmnet(X , Y , family = "gaussian" ,
                                          lambda = lambda.try , alpha = 0.5 ,
                                          penalty.factor = 1 / abs(best.ridge.coefs)^nu.try[i])
              lambda.adaelnet5.opt <- adaelnet5.model$lambda.min
              best.adaelnet5.coefs <- predict(adaelnet5.model , type = "coefficients" ,
                                             s = lambda.adaelnet5.opt)[-1]
              adaelnet5.nu.cv[[i]] <- list(model = list(full.model = adaelnet5.model , 
                                                       lambda = lambda.adaelnet5.opt , 
                                                       coefs = best.adaelnet5.coefs) , 
                                          metrics_and_info = list(#model.seed.ridge = seed.ridge ,
                                                                  #model.seed.prenu = seed.pre.nu , 
                                                                  #model.seed.nu = seed ,
                                                                  ridge.coefs = best.ridge.coefs ,
                                                                  weights = 1 / abs(best.ridge.coefs)^nu.try[i] , 
                                                                  nu = nu.try[i] , 
                                                                  lambda = lambda.adaelnet5.opt , 
                                                                  coefs = best.adaelnet5.coefs , 
                                                                  mpe = adaelnet5.model$cvm[which(adaelnet5.model$lambda == lambda.adaelnet5.opt)] , 
                                                                  mpe.sd = adaelnet5.model$cvsd[which(adaelnet5.model$lambda == lambda.adaelnet5.opt)] , 
                                                                  fpr = length(which(best.adaelnet5.coefs[c(5:p)] != 0)) / length(best.adaelnet5.coefs[c(5:p)]) , 
                                                                  fnr = length(which(best.adaelnet5.coefs[c(1:4)] == 0)) / length(best.adaelnet5.coefs[1:4])))
       }
       #find minimizing nu/gamma
       adaelnet5.nu.cv.mpe <- numeric()
       #adaelnet5.seeds.ridge <- numeric()
       #adaelnet5.seeds.prenu <- numeric()
       #adaelnet5.seeds.nu <- numeric()
       for(i in 1:length(adaelnet5.nu.cv)) {
              adaelnet5.nu.cv.mpe[i] <- adaelnet5.nu.cv[[i]]$metrics_and_info$mpe
              #adaelnet5.seeds.ridge[i] <- adaelnet5.nu.cv[[i]]$metrics_and_info$model.seed.ridge
              #adaelnet5.seeds.prenu[i] <- adaelnet5.nu.cv[[i]]$metrics_and_info$model.seed.prenu
              #adaelnet5.seeds.nu[i] <- adaelnet5.nu.cv[[i]]$metrics_and_info$model.seed.nu
       }

       #return(adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]])
       #store BEST adaelnet5 result plus all seeds
       ###below is used to check that seeds are regenerated properly and not uniform
       return(list(mpes = adaelnet5.nu.cv.mpe , 
                   #seeds.ridge = adaelnet5.seeds.ridge , 
                   #seeds.prenu = adaelnet5.seeds.prenu , 
                   #seeds.nu = adaelnet5.seeds.nu ,  
                   model = adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]] , 
                   important = list(diagnostics = data.frame(cbind(data.seed = tracker[7])) ,
                                                                   #model.seed.ridge = adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge ,
                                                                   #model.seed.prenu = adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                                   #model.seed.nu = adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$model.seed.nu)) , 
                                    coefs = adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$coefs , 
                                    weights = adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$weights ,
                                    info = data.frame(cbind(n = tracker[1] , 
                                                            p = tracker[2] , 
                                                            eta.x = tracker[3] , 
                                                            eta.y = tracker[4] , 
                                                            g = tracker[5] , 
                                                            h = tracker[6] , 
                                                            data.seed = tracker[7] ,
                                                            #model.seed.ridge = adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge , 
                                                            #model.seed.prenu = adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                            #model.seed.nu = adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$model.seed.nu , 
                                                            alpha = 0.5 , 
                                                            lambda = adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$lambda ,
                                                            nu = adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$nu ,
                                                            mpe = adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$mpe , 
                                                            mpe.sd = adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$mpe.sd , 
                                                            fpr = adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$fpr , 
                                                            fnr = adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$fnr
                                                            )
                                                      )
                                    )
                   )
              )
}


#run across debug dataset
adaelnet5.half <- half.data %>%   
       map(safely(adaelnet5.sim.fnct))

saveRDS(adaelnet5.half , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/adaelnet5_500.RData")


