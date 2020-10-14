#load libraries
library(quantreg)
library(glmnet)
library(magrittr)
library(purrr)
library(msaenet)

#load data
#data.half <- readRDS()
#full.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/fulldata_091620.RData")
#half.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/500_data_10052020.RData")
#testing10.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/testing10_data_091720.RData")
#single.data <- testing10.data[[1]]
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")


#adaptive lasso function with two-way CV for selecting both lambda and nu/gamma
msaelnet5.sim.fnct <- function(data) { 
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
       ##grid of nu/gamma values to try
       nu.try <- exp(seq(log(0.01) , log(10) , length.out = 100))
       #seed.pre.nu <- data$seeds[ , "seed.15"]
       #set.seed(seed.pre.nu)
       #seed.nu <- sample(rnorm(n = 1000000000) , size = length(nu.try) , replace = FALSE)
       ##initialize list of best msaelnet5 results from each nu/gamma
       msaelnet5.nu.cv <- list()
       for(i in 1:length(nu.try)) {
              #seed <- seed.nu[i]
              #set.seed(seed)
              #single adaptive lasso run with ridge weighting and nu = 1
              msaelnet5.model <- msaenet(x = X , y = Y , family = "gaussian" , 
                                         init = "ridge" , alphas = 0.5 , 
                                         tune = "cv" , nfolds = 5L , 
                                         rule = "lambda.min" , nsteps = 10L , 
                                         tune.nsteps = "max" , scale = nu.try[i])
              lambda.msaelnet5.opt <- msaelnet5.model[["best.lambdas"]][[11]]
              best.msaelnet5.coefs <- coef(msaelnet5.model) #coefficients
              msaelnet5.nu.cv[[i]] <- list(model = list(full.model = msaelnet5.model , 
                                                       lambda = lambda.msaelnet5.opt , 
                                                       coefs = best.msaelnet5.coefs) , 
                                          metrics_and_info = list(#model.seed.ridge = seed.ridge ,
                                                                  #model.seed.prenu = seed.pre.nu , 
                                                                  #model.seed.nu = seed ,
                                                                  weights = msaelnet5.model[["adapen.list"]][[10]] , 
                                                                  nu = nu.try[i] , 
                                                                  lambda = lambda.msaelnet5.opt , 
                                                                  coefs = best.msaelnet5.coefs , 
                                                                  mpe = msaelnet5.model[["step.criterion"]][[11]] , 
                                                                  #mpe.sd = msaelnet5.model$cvsd[which(msaelnet5.model$lambda == lambda.msaelnet5.opt)] , 
                                                                  fpr = length(which(best.msaelnet5.coefs[c(5:p)] != 0)) / length(best.msaelnet5.coefs[c(5:p)]) , 
                                                                  fnr = length(which(best.msaelnet5.coefs[c(1:4)] == 0)) / length(best.msaelnet5.coefs[1:4])))
       }
       #find minimizing nu/gamma
       msaelnet5.nu.cv.mpe <- numeric()
       #msaelnet5.seeds.ridge <- numeric()
       #msaelnet5.seeds.prenu <- numeric()
       #msaelnet5.seeds.nu <- numeric()
       for(i in 1:length(msaelnet5.nu.cv)) {
              msaelnet5.nu.cv.mpe[i] <- msaelnet5.nu.cv[[i]]$metrics_and_info$mpe
              #msaelnet5.seeds.ridge[i] <- msaelnet5.nu.cv[[i]]$metrics_and_info$model.seed.ridge
              #msaelnet5.seeds.prenu[i] <- msaelnet5.nu.cv[[i]]$metrics_and_info$model.seed.prenu
              #msaelnet5.seeds.nu[i] <- msaelnet5.nu.cv[[i]]$metrics_and_info$model.seed.nu
       }

       #return(msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]])
       #store BEST msaelnet5 result plus all seeds
       ###below is used to check that seeds are regenerated properly and not uniform
       return(list(mpes = msaelnet5.nu.cv.mpe , 
                   #seeds.ridge = msaelnet5.seeds.ridge , 
                   #seeds.prenu = msaelnet5.seeds.prenu , 
                   #seeds.nu = msaelnet5.seeds.nu ,  
                   model = msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]] , 
                   important = list(diagnostics = data.frame(cbind(data.seed = tracker[7])) ,
                                                                   #model.seed.ridge = msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge ,
                                                                   #model.seed.prenu = msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                                   #model.seed.nu = msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]]$metrics_and_info$model.seed.nu)) , 
                                    coefs = msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]]$metrics_and_info$coefs , 
                                    weights = msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]]$metrics_and_info$weights ,
                                    info = data.frame(cbind(n = tracker[1] , 
                                                            p = tracker[2] , 
                                                            eta.x = tracker[3] , 
                                                            eta.y = tracker[4] , 
                                                            g = tracker[5] , 
                                                            h = tracker[6] , 
                                                            data.seed = tracker[7] ,
                                                            #model.seed.ridge = msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge , 
                                                            #model.seed.prenu = msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                            #model.seed.nu = msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]]$metrics_and_info$model.seed.nu , 
                                                            alpha = 0.5 , 
                                                            lambda = msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]]$metrics_and_info$lambda ,
                                                            nu = msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]]$metrics_and_info$nu ,
                                                            mpe = msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]]$metrics_and_info$mpe , 
                                                            mpe.sd = msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]]$metrics_and_info$mpe.sd , 
                                                            fpr = msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]]$metrics_and_info$fpr , 
                                                            fnr = msaelnet5.nu.cv[[which.min(msaelnet5.nu.cv.mpe)]]$metrics_and_info$fnr
                                                            )
                                                      )
                                    )
                   )
              )
}


#run across debug dataset
msaelnet5.debug <- debug.data %>%   
       map(safely(msaelnet5.sim.fnct))

#saveRDS(msaelnet5.half , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/msaelnet5_DEBUG.RData")


