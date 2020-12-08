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
distr50.halfdata <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/distr50.RData")
#testing10.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/testing10_data_091720.RData")
#single.data <- testing10.data[[1]]
#debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")


#adaptive lasso function with two-way CV for selecting both lambda and nu/gamma
msaelnet9.sim.fnct <- function(data) { 
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
       ##initialize list of best msaelnet9 results from each nu/gamma
       msaelnet9.nu.cv <- list()
       for(i in 1:length(nu.try)) {
              #seed <- seed.nu[i]
              #set.seed(seed)
              #single adaptive lasso run with ridge weighting and nu = 1
              msaelnet9.model <- msaenet(x = X , y = Y , family = "gaussian" , 
                                         init = "ridge" , alphas = 0.9 , 
                                         tune = "cv" , nfolds = 5L , 
                                         rule = "lambda.min" , nsteps = 10L , 
                                         tune.nsteps = "max" , scale = nu.try[i])
              lambda.msaelnet9.opt <- msaelnet9.model[["best.lambdas"]][[11]]
              best.msaelnet9.coefs <- coef(msaelnet9.model) #coefficients
              msaelnet9.nu.cv[[i]] <- list(model = list(full.model = msaelnet9.model , 
                                                       lambda = lambda.msaelnet9.opt , 
                                                       coefs = best.msaelnet9.coefs) , 
                                          metrics_and_info = list(#model.seed.ridge = seed.ridge ,
                                                                  #model.seed.prenu = seed.pre.nu , 
                                                                  #model.seed.nu = seed ,
                                                                  weights = msaelnet9.model[["adapen.list"]][[10]] , 
                                                                  nu = nu.try[i] , 
                                                                  lambda = lambda.msaelnet9.opt , 
                                                                  coefs = best.msaelnet9.coefs , 
                                                                  mpe = msaelnet9.model[["step.criterion"]][[11]] , 
                                                                  #mpe.sd = msaelnet9.model$cvsd[which(msaelnet9.model$lambda == lambda.msaelnet9.opt)] , 
                                                                  fpr = length(which(best.msaelnet9.coefs[c(5:p)] != 0)) / length(best.msaelnet9.coefs[c(5:p)]) , 
                                                                  fnr = length(which(best.msaelnet9.coefs[c(1:4)] == 0)) / length(best.msaelnet9.coefs[1:4])))
       }
       #find minimizing nu/gamma
       msaelnet9.nu.cv.mpe <- numeric()
       #msaelnet9.seeds.ridge <- numeric()
       #msaelnet9.seeds.prenu <- numeric()
       #msaelnet9.seeds.nu <- numeric()
       for(i in 1:length(msaelnet9.nu.cv)) {
              msaelnet9.nu.cv.mpe[i] <- msaelnet9.nu.cv[[i]]$metrics_and_info$mpe
              #msaelnet9.seeds.ridge[i] <- msaelnet9.nu.cv[[i]]$metrics_and_info$model.seed.ridge
              #msaelnet9.seeds.prenu[i] <- msaelnet9.nu.cv[[i]]$metrics_and_info$model.seed.prenu
              #msaelnet9.seeds.nu[i] <- msaelnet9.nu.cv[[i]]$metrics_and_info$model.seed.nu
       }

       #return(msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]])
       #store BEST msaelnet9 result plus all seeds
       ###below is used to check that seeds are regenerated properly and not uniform
       return(list(mpes = msaelnet9.nu.cv.mpe , 
                   #seeds.ridge = msaelnet9.seeds.ridge , 
                   #seeds.prenu = msaelnet9.seeds.prenu , 
                   #seeds.nu = msaelnet9.seeds.nu ,  
                   model = msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]] , 
                   important = list(diagnostics = data.frame(cbind(data.seed = tracker[7])) ,
                                                                   #model.seed.ridge = msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge ,
                                                                   #model.seed.prenu = msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                                   #model.seed.nu = msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]]$metrics_and_info$model.seed.nu)) , 
                                    coefs = msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]]$metrics_and_info$coefs , 
                                    weights = msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]]$metrics_and_info$weights ,
                                    info = data.frame(cbind(n = tracker[1] , 
                                                            p = tracker[2] , 
                                                            eta.x = tracker[3] , 
                                                            eta.y = tracker[4] , 
                                                            g = tracker[5] , 
                                                            h = tracker[6] , 
                                                            data.seed = tracker[7] ,
                                                            #model.seed.ridge = msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge , 
                                                            #model.seed.prenu = msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                            #model.seed.nu = msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]]$metrics_and_info$model.seed.nu , 
                                                            alpha = 0.9 , 
                                                            lambda = msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]]$metrics_and_info$lambda ,
                                                            nu = msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]]$metrics_and_info$nu ,
                                                            mpe = msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]]$metrics_and_info$mpe , 
                                                            mpe.sd = msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]]$metrics_and_info$mpe.sd , 
                                                            fpr = msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]]$metrics_and_info$fpr , 
                                                            fnr = msaelnet9.nu.cv[[which.min(msaelnet9.nu.cv.mpe)]]$metrics_and_info$fnr
                                                            )
                                                      )
                                    )
                   )
              )
}


#run across debug dataset
msaelnet9.distr50.half <- distr50.halfdata %>%   
       map(safely(msaelnet9.sim.fnct))

saveRDS(msaelnet9.distr50.half , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet9_distr50_500.RData")


