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
outlier50.halfdata <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/outlier50.RData")
#testing10.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/testing10_data_091720.RData")
#single.data <- testing10.data[[1]]
#debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")


#adaptive lasso function with two-way CV for selecting both lambda and nu/gamma
msaelnet75.sim.fnct <- function(data) { 
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
       ##initialize list of best msaelnet75 results from each nu/gamma
       msaelnet75.nu.cv <- list()
       for(i in 1:length(nu.try)) {
              #seed <- seed.nu[i]
              #set.seed(seed)
              #single adaptive lasso run with ridge weighting and nu = 1
              msaelnet75.model <- msaenet(x = X , y = Y , family = "gaussian" , 
                                         init = "ridge" , alphas = 0.75 , 
                                         tune = "cv" , nfolds = 5L , 
                                         rule = "lambda.min" , nsteps = 10L , 
                                         tune.nsteps = "max" , scale = nu.try[i])
              lambda.msaelnet75.opt <- msaelnet75.model[["best.lambdas"]][[11]]
              best.msaelnet75.coefs <- coef(msaelnet75.model) #coefficients
              msaelnet75.nu.cv[[i]] <- list(model = list(full.model = msaelnet75.model , 
                                                       lambda = lambda.msaelnet75.opt , 
                                                       coefs = best.msaelnet75.coefs) , 
                                          metrics_and_info = list(#model.seed.ridge = seed.ridge ,
                                                                  #model.seed.prenu = seed.pre.nu , 
                                                                  #model.seed.nu = seed ,
                                                                  weights = msaelnet75.model[["adapen.list"]][[10]] , 
                                                                  nu = nu.try[i] , 
                                                                  lambda = lambda.msaelnet75.opt , 
                                                                  coefs = best.msaelnet75.coefs , 
                                                                  mpe = msaelnet75.model[["step.criterion"]][[11]] , 
                                                                  #mpe.sd = msaelnet75.model$cvsd[which(msaelnet75.model$lambda == lambda.msaelnet75.opt)] , 
                                                                  fpr = length(which(best.msaelnet75.coefs[c(5:p)] != 0)) / length(best.msaelnet75.coefs[c(5:p)]) , 
                                                                  fnr = length(which(best.msaelnet75.coefs[c(1:4)] == 0)) / length(best.msaelnet75.coefs[1:4])))
       }
       #find minimizing nu/gamma
       msaelnet75.nu.cv.mpe <- numeric()
       #msaelnet75.seeds.ridge <- numeric()
       #msaelnet75.seeds.prenu <- numeric()
       #msaelnet75.seeds.nu <- numeric()
       for(i in 1:length(msaelnet75.nu.cv)) {
              msaelnet75.nu.cv.mpe[i] <- msaelnet75.nu.cv[[i]]$metrics_and_info$mpe
              #msaelnet75.seeds.ridge[i] <- msaelnet75.nu.cv[[i]]$metrics_and_info$model.seed.ridge
              #msaelnet75.seeds.prenu[i] <- msaelnet75.nu.cv[[i]]$metrics_and_info$model.seed.prenu
              #msaelnet75.seeds.nu[i] <- msaelnet75.nu.cv[[i]]$metrics_and_info$model.seed.nu
       }

       #return(msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]])
       #store BEST msaelnet75 result plus all seeds
       ###below is used to check that seeds are regenerated properly and not uniform
       return(list(mpes = msaelnet75.nu.cv.mpe , 
                   #seeds.ridge = msaelnet75.seeds.ridge , 
                   #seeds.prenu = msaelnet75.seeds.prenu , 
                   #seeds.nu = msaelnet75.seeds.nu ,  
                   model = msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]] , 
                   important = list(diagnostics = data.frame(cbind(data.seed = tracker[7])) ,
                                                                   #model.seed.ridge = msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge ,
                                                                   #model.seed.prenu = msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                                   #model.seed.nu = msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]]$metrics_and_info$model.seed.nu)) , 
                                    coefs = msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]]$metrics_and_info$coefs , 
                                    weights = msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]]$metrics_and_info$weights ,
                                    info = data.frame(cbind(n = tracker[1] , 
                                                            p = tracker[2] , 
                                                            eta.x = tracker[3] , 
                                                            eta.y = tracker[4] , 
                                                            g = tracker[5] , 
                                                            h = tracker[6] , 
                                                            data.seed = tracker[7] ,
                                                            #model.seed.ridge = msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge , 
                                                            #model.seed.prenu = msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                            #model.seed.nu = msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]]$metrics_and_info$model.seed.nu , 
                                                            alpha = 0.75 , 
                                                            lambda = msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]]$metrics_and_info$lambda ,
                                                            nu = msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]]$metrics_and_info$nu ,
                                                            mpe = msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]]$metrics_and_info$mpe , 
                                                            mpe.sd = msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]]$metrics_and_info$mpe.sd , 
                                                            fpr = msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]]$metrics_and_info$fpr , 
                                                            fnr = msaelnet75.nu.cv[[which.min(msaelnet75.nu.cv.mpe)]]$metrics_and_info$fnr
                                                            )
                                                      )
                                    )
                   )
              )
}


#run across debug dataset
msaelnet75.outlier50.half <- outlier50.halfdata %>%   
       map(safely(msaelnet75.sim.fnct))

saveRDS(msaelnet75.outlier50.half , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet75_outlier50_500.RData")


