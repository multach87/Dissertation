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
adaelnet9.sim.fnct <- function(data , alpha = 0.9) { 
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
       seed.ridge <- data$seeds[ , "seed.18"]
       set.seed(seed.ridge)
       #ridge coefs for weighting
       lambda.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       ridge.model <- cv.glmnet(x = X , y = Y , lambda = lambda.try , alpha = 0)
       lambda.ridge.opt <- ridge.model$lambda.min
       best.ridge.coefs <- predict(ridge.model , type = "coefficients" ,
                                   s = lambda.ridge.opt)[-1]
       ##grid of nu/gamma values to try
       nu.try <- exp(seq(log(0.01) , log(10) , length.out = 100))
       seed.pre.nu <- data$seeds[ , "seed.19"]
       set.seed(seed.pre.nu)
       seed.nu <- sample(rnorm(n = 1000000000) , size = length(nu.try) , replace = FALSE)
       ##initialize list of best adaelnet9 results from each nu/gamma
       adaelnet9.nu.cv <- list()
       for(i in 1:length(nu.try)) {
              seed <- seed.nu[i]
              set.seed(seed)
              #single adaptive lasso run with ridge weighting and nu = 1
              adaelnet9.model <- cv.glmnet(X , Y , family = "gaussian" ,
                                          lambda = lambda.try , alpha = alpha ,
                                          penalty.factor = 1 / abs(best.ridge.coefs)^nu.try[i])
              lambda.adaelnet9.opt <- adaelnet9.model$lambda.min
              best.adaelnet9.coefs <- predict(adaelnet9.model , type = "coefficients" ,
                                             s = lambda.adaelnet9.opt)[-1]
              adaelnet9.nu.cv[[i]] <- list(model = list(full.model = adaelnet9.model , 
                                                       lambda = lambda.adaelnet9.opt , 
                                                       coefs = best.adaelnet9.coefs) , 
                                          metrics_and_info = list(model.seed.ridge = seed.ridge ,
                                                                  model.seed.prenu = seed.pre.nu , 
                                                                  model.seed.nu = seed ,
                                                                  ridge.coefs = best.ridge.coefs ,
                                                                  weights = 1 / abs(best.ridge.coefs)^nu.try[i] , 
                                                                  nu = nu.try[i] , 
                                                                  lambda = lambda.adaelnet9.opt , 
                                                                  coefs = best.adaelnet9.coefs , 
                                                                  mpe = adaelnet9.model$cvm[which(adaelnet9.model$lambda == lambda.adaelnet9.opt)] , 
                                                                  mpe.sd = adaelnet9.model$cvsd[which(adaelnet9.model$lambda == lambda.adaelnet9.opt)] , 
                                                                  fpr = length(which(best.adaelnet9.coefs[c(5:p)] != 0)) / length(best.adaelnet9.coefs[c(5:p)]) , 
                                                                  fnr = length(which(best.adaelnet9.coefs[c(1:4)] == 0)) / length(best.adaelnet9.coefs[1:4])))
       }
       #find minimizing nu/gamma
       adaelnet9.nu.cv.mpe <- numeric()
       adaelnet9.seeds.ridge <- numeric()
       adaelnet9.seeds.prenu <- numeric()
       adaelnet9.seeds.nu <- numeric()
       for(i in 1:length(adaelnet9.nu.cv)) {
              adaelnet9.nu.cv.mpe[i] <- adaelnet9.nu.cv[[i]]$metrics_and_info$mpe
              adaelnet9.seeds.ridge[i] <- adaelnet9.nu.cv[[i]]$metrics_and_info$model.seed.ridge
              adaelnet9.seeds.prenu[i] <- adaelnet9.nu.cv[[i]]$metrics_and_info$model.seed.prenu
              adaelnet9.seeds.nu[i] <- adaelnet9.nu.cv[[i]]$metrics_and_info$model.seed.nu
       }

       #return(adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]])
       #store BEST adaelnet9 result plus all seeds
       ###below is used to check that seeds are regenerated properly and not uniform
       return(list(mpes = adaelnet9.nu.cv.mpe , 
                   seeds.ridge = adaelnet9.seeds.ridge , 
                   seeds.prenu = adaelnet9.seeds.prenu , 
                   seeds.nu = adaelnet9.seeds.nu ,  
                   model = adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]] , 
                   important = list(diagnostics = data.frame(cbind(data.seed = tracker[7] ,
                                                                   model.seed.ridge = adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge ,
                                                                   model.seed.prenu = adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                                   model.seed.nu = adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]]$metrics_and_info$model.seed.nu)) , 
                                    coefs = adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]]$metrics_and_info$coefs , 
                                    weights = adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]]$metrics_and_info$weights ,
                                    info = data.frame(cbind(n = tracker[1] , 
                                                            p = tracker[2] , 
                                                            eta.x = tracker[3] , 
                                                            eta.y = tracker[4] , 
                                                            g = tracker[5] , 
                                                            h = tracker[6] , 
                                                            data.seed = tracker[7] ,
                                                            model.seed.ridge = adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge , 
                                                            model.seed.prenu = adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                            model.seed.nu = adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]]$metrics_and_info$model.seed.nu , 
                                                            alpha = alpha , 
                                                            lambda = adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]]$metrics_and_info$lambda ,
                                                            nu = adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]]$metrics_and_info$nu ,
                                                            mpe = adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]]$metrics_and_info$mpe , 
                                                            mpe.sd = adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]]$metrics_and_info$mpe.sd , 
                                                            fpr = adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]]$metrics_and_info$fpr , 
                                                            fnr = adaelnet9.nu.cv[[which.min(adaelnet9.nu.cv.mpe)]]$metrics_and_info$fnr
                                                            )
                                                      )
                                    )
                   )
              )
}


#run across debug dataset
adaelnet9.debug <- debug.data %>%   
       map(safely(adaelnet9.sim.fnct))

saveRDS(adaelnet9.debug , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/std_adaelnet9_DEBUG.RData")


