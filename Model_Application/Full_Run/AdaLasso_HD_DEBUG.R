#load libraries
library(quantreg)
library(glmnet)
library(magrittr)
library(purrr)

#load data
HD.data_DEBUG <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")

#adaptive lasso function with two-way CV for selecting both lambda and nu/gamma
adalasso.sim.fnct <- function(data) { 
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
       seed.ridge <- data$seeds[ , "seed.2"]
       set.seed(seed.ridge)
       #ridge coefs for weighting
       lambda.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       ridge.model <- cv.glmnet(x = X , y = Y , lambda = lambda.try , alpha = 0)
       lambda.ridge.opt <- ridge.model$lambda.min
       best.ridge.coefs <- predict(ridge.model , type = "coefficients" ,
                                   s = lambda.ridge.opt)[-1]
       ##grid of nu/gamma values to try
       nu.try <- exp(seq(log(0.01) , log(10) , length.out = 100))
       #seed.pre.nu <- data$seeds[ , "seed.3"]
       #set.seed(seed.pre.nu)
       #seed.nu <- sample(rnorm(n = 1000000000) , size = length(nu.try) , replace = FALSE)
       ##initialize list of best adalasso results from each nu/gamma
       adalasso.nu.cv <- list()
       for(i in 1:length(nu.try)) {
              #seed <- seed.nu[i]
              #set.seed(seed)
              #single adaptive lasso run with ridge weighting and nu = 1
              adalasso.model <- cv.glmnet(X , Y , family = "gaussian" ,
                                          lambda = lambda.try , 
                                          penalty.factor = 1 / abs(best.ridge.coefs)^nu.try[i])
              lambda.adalasso.opt <- adalasso.model$lambda.min
              best.adalasso.coefs <- predict(adalasso.model , type = "coefficients" ,
                                             s = lambda.adalasso.opt)[-1]
              adalasso.nu.cv[[i]] <- list(model = list(full.model = adalasso.model , 
                                                       lambda = lambda.adalasso.opt , 
                                                       coefs = best.adalasso.coefs) , 
                                          metrics_and_info = list(model.seed.ridge = seed.ridge ,
                                                                  #model.seed.prenu = seed.pre.nu , 
                                                                  #model.seed.nu = seed ,
                                                                  ridge.coefs = best.ridge.coefs ,
                                                                  weights = 1 / abs(best.ridge.coefs)^nu.try[i] , 
                                                                  nu = nu.try[i] , 
                                                                  lambda = lambda.adalasso.opt , 
                                                                  coefs = best.adalasso.coefs , 
                                                                  mpe = adalasso.model$cvm[which(adalasso.model$lambda == lambda.adalasso.opt)] , 
                                                                  mpe.sd = adalasso.model$cvsd[which(adalasso.model$lambda == lambda.adalasso.opt)] , 
                                                                  fpr = length(which(best.adalasso.coefs[c(5:p)] != 0)) / length(best.adalasso.coefs[c(5:p)]) , 
                                                                  fnr = length(which(best.adalasso.coefs[c(1:4)] == 0)) / length(best.adalasso.coefs[1:4])))
       }
       #find minimizing nu/gamma
       adalasso.nu.cv.mpe <- numeric()
       adalasso.seeds.ridge <- numeric()
       #adalasso.seeds.prenu <- numeric()
       #adalasso.seeds.nu <- numeric()
       for(i in 1:length(adalasso.nu.cv)) {
              adalasso.nu.cv.mpe[i] <- adalasso.nu.cv[[i]]$metrics_and_info$mpe
              adalasso.seeds.ridge[i] <- adalasso.nu.cv[[i]]$metrics_and_info$model.seed.ridge
              #adalasso.seeds.prenu[i] <- adalasso.nu.cv[[i]]$metrics_and_info$model.seed.prenu
              #adalasso.seeds.nu[i] <- adalasso.nu.cv[[i]]$metrics_and_info$model.seed.nu
       }

       #return(adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]])
       #store BEST adalasso result plus all seeds
       ###below is used to check that seeds are regenerated properly and not uniform
       return(list(mpes = adalasso.nu.cv.mpe , 
                   seeds.ridge = adalasso.seeds.ridge , 
                   #seeds.prenu = adalasso.seeds.prenu , 
                   #seeds.nu = adalasso.seeds.nu ,  
                   model = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]] , 
                   important = list(diagnostics = data.frame(cbind(data.seed = tracker[7] ,
                                                                   model.seed.ridge = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge)) ,
                                                                   #model.seed.prenu = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                                   #model.seed.nu = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$model.seed.nu)) , 
                                    coefs = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$coefs , 
                                    weights = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$weights ,
                                    info = data.frame(cbind(n = tracker[1] , 
                                                            p = tracker[2] , 
                                                            eta.x = tracker[3] , 
                                                            eta.y = tracker[4] , 
                                                            g = tracker[5] , 
                                                            h = tracker[6] , 
                                                            data.seed = tracker[7] ,
                                                            model.seed.ridge = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge , 
                                                            #model.seed.prenu = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                            #model.seed.nu = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$model.seed.nu , 
                                                            lambda = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$lambda ,
                                                            nu = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$nu ,
                                                            mpe = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$mpe , 
                                                            mpe.sd = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$mpe.sd , 
                                                            fpr = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$fpr , 
                                                            fnr = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$fnr
                                                            )
                                                      )
                                    )
                   )
              )
}


#run across full dataset
#run across full dataset
adalasso.HD.debug <- HD.data_DEBUG %>%   
       map(safely(adalasso.sim.fnct))

saveRDS(adalasso.HD.debug , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/adalasso_HD_DEBUG.RData")

