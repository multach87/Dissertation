#load libraries
library(quantreg)
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.half <- readRDS()
#full.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/fulldata_091620.RData")
HD.data_DEBUG <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")



#adaptive lasso function with two-way CV for selecting both lambda and nu/gamma
adaelnet75.sim.fnct <- function(data) { 
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
       ##initialize list of best adaelnet75 results from each nu/gamma
       adaelnet75.nu.cv <- list()
       for(i in 1:length(nu.try)) {
              #seed <- seed.nu[i]
              #set.seed(seed)
              #single adaptive lasso run with ridge weighting and nu = 1
              adaelnet75.model <- cv.glmnet(X , Y , family = "gaussian" ,
                                          lambda = lambda.try , alpha = 0.75 ,
                                          penalty.factor = 1 / abs(best.ridge.coefs)^nu.try[i])
              lambda.adaelnet75.opt <- adaelnet75.model$lambda.min
              best.adaelnet75.coefs <- predict(adaelnet75.model , type = "coefficients" ,
                                             s = lambda.adaelnet75.opt)[-1]
              adaelnet75.nu.cv[[i]] <- list(model = list(full.model = adaelnet75.model , 
                                                       lambda = lambda.adaelnet75.opt , 
                                                       coefs = best.adaelnet75.coefs) , 
                                          metrics_and_info = list(#model.seed.ridge = seed.ridge ,
                                                                  #model.seed.prenu = seed.pre.nu , 
                                                                  #model.seed.nu = seed ,
                                                                  ridge.coefs = best.ridge.coefs ,
                                                                  weights = 1 / abs(best.ridge.coefs)^nu.try[i] , 
                                                                  nu = nu.try[i] , 
                                                                  lambda = lambda.adaelnet75.opt , 
                                                                  coefs = best.adaelnet75.coefs , 
                                                                  mpe = adaelnet75.model$cvm[which(adaelnet75.model$lambda == lambda.adaelnet75.opt)] , 
                                                                  mpe.sd = adaelnet75.model$cvsd[which(adaelnet75.model$lambda == lambda.adaelnet75.opt)] , 
                                                                  fpr = length(which(best.adaelnet75.coefs[c(5:p)] != 0)) / length(best.adaelnet75.coefs[c(5:p)]) , 
                                                                  fnr = length(which(best.adaelnet75.coefs[c(1:4)] == 0)) / length(best.adaelnet75.coefs[1:4])))
       }
       #find minimizing nu/gamma
       adaelnet75.nu.cv.mpe <- numeric()
       #adaelnet75.seeds.ridge <- numeric()
       #adaelnet75.seeds.prenu <- numeric()
       #adaelnet75.seeds.nu <- numeric()
       for(i in 1:length(adaelnet75.nu.cv)) {
              adaelnet75.nu.cv.mpe[i] <- adaelnet75.nu.cv[[i]]$metrics_and_info$mpe
              #adaelnet75.seeds.ridge[i] <- adaelnet75.nu.cv[[i]]$metrics_and_info$model.seed.ridge
              #adaelnet75.seeds.prenu[i] <- adaelnet75.nu.cv[[i]]$metrics_and_info$model.seed.prenu
              #adaelnet75.seeds.nu[i] <- adaelnet75.nu.cv[[i]]$metrics_and_info$model.seed.nu
       }

       #return(adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]])
       #store BEST adaelnet75 result plus all seeds
       ###below is used to check that seeds are regenerated properly and not uniform
       return(list(mpes = adaelnet75.nu.cv.mpe , 
                   #seeds.ridge = adaelnet75.seeds.ridge , 
                   #seeds.prenu = adaelnet75.seeds.prenu , 
                   #seeds.nu = adaelnet75.seeds.nu ,  
                   model = adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]] , 
                   important = list(diagnostics = data.frame(cbind(data.seed = tracker[7])) ,
                                                                   #model.seed.ridge = adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge ,
                                                                   #model.seed.prenu = adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                                   #model.seed.nu = adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]]$metrics_and_info$model.seed.nu)) , 
                                    coefs = adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]]$metrics_and_info$coefs , 
                                    weights = adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]]$metrics_and_info$weights ,
                                    info = data.frame(cbind(n = tracker[1] , 
                                                            p = tracker[2] , 
                                                            eta.x = tracker[3] , 
                                                            eta.y = tracker[4] , 
                                                            g = tracker[5] , 
                                                            h = tracker[6] , 
                                                            data.seed = tracker[7] ,
                                                            #model.seed.ridge = adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge , 
                                                            #model.seed.prenu = adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                            #model.seed.nu = adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]]$metrics_and_info$model.seed.nu , 
                                                            alpha = 0.75 , 
                                                            lambda = adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]]$metrics_and_info$lambda ,
                                                            nu = adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]]$metrics_and_info$nu ,
                                                            mpe = adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]]$metrics_and_info$mpe , 
                                                            mpe.sd = adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]]$metrics_and_info$mpe.sd , 
                                                            fpr = adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]]$metrics_and_info$fpr , 
                                                            fnr = adaelnet75.nu.cv[[which.min(adaelnet75.nu.cv.mpe)]]$metrics_and_info$fnr
                                                            )
                                                      )
                                    )
                   )
              )
}


#run across debug dataset
adaelnet75.HD.debug <- HD.data_DEBUG %>%   
       map(safely(adaelnet75.sim.fnct))

saveRDS(adaelnet75.HD.debug , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/adaelnet75_HD_DEBUG.RData")
