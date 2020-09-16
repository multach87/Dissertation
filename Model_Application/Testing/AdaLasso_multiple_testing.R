#load libraries
library(quantreg)
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.full <- readRDS()
testing10.data <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Dissertation_Git/Data_Generation/Data_Storage/testing10_data_091520.RData")


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
       seed.ridge <- sample(sample(x = c(1:1000000000) , size = 1000 , replace = FALSE) , size = 1)
       set.seed(seed.ridge)
       #ridge coefs for weighting
       lambda.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       ridge.model <- cv.glmnet(x = X , y = Y , lambda = lambda.try , alpha = 0)
       lambda.ridge.opt <- ridge.model$lambda.min
       best.ridge.coefs <- predict(ridge.model , type = "coefficients" ,
                                   s = lambda.ridge.opt)[-1]
       ##grid of nu/gamma values to try
       nu.try <- exp(seq(log(0.01) , log(10) , length.out = 100))
       seed.nu <- sample(rnorm(n = 1000000000) , size = length(nu.try) , replace = FALSE)
       ##initialize list of best adalasso results from each nu/gamma
       adalasso.nu.cv <- list()
       for(i in 1:length(nu.try)) {
              seed <- seed.nu[i]
              set.seed(seed)
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
                                                                  model.seed.nu = seed ,
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
       for(i in 1:length(adalasso.nu.cv)) {
              adalasso.nu.cv.mpe[i] <- adalasso.nu.cv[[i]]$metrics_and_info$mpe
       }
       adalass.seeds.ridge <- numeric()
       adalass.seeds.nu <- numeric()
       for(i in 1:length(adalasso.nu.cv)) {
              adalass.seeds.ridge[i] <- adalasso.nu.cv[[i]]$metrics_and_info$model.seed.ridge
              adalass.seeds.nu[i] <- adalasso.nu.cv[[i]]$metrics_and_info$model.seed.nu
       }
       #return(adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]])
       #store BEST adalasso result plus all seeds
       ###below is used to check that seeds are regenerated properly and not uniform
       return(list(mpes = adalasso.nu.cv.mpe , 
                   seeds.ridge = adalass.seeds.ridge , 
                   seeds.nu = adalass.seeds.nu ,  
                   model = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]] , 
                   important = list(diagnostics = data.frame(cbind(data.seed = tracker[7] ,
                                                                   model.seed.ridge = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge , 
                                                                   model.seed.nu = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$model.seed.nu)) , 
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
                                                            model.seed.nu = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$model.seed.nu , 
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
adalasso.full <- testing10.data %>%   
       map(safely(adalasso.sim.fnct))

#dealing with error/result from map(safely())
#create empty lists for error + result
adalasso.error <- list()
adalasso.result <- list()
adalasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(adalasso.full)) { 
       #iteration tracker
       cat("i = " , i , "\n")
       #fill error list
       adalasso.error[[i]] <- list(error = adalasso.full[[i]]$error , 
                                   condition = as.data.frame(unlist(testing10.data[[i]]$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
       #fill in results if results aren't NULL from safely()
       adalasso.result[[i]] <- adalasso.full[[i]]$result
       #fill final list
       if(!is.null(adalasso.full[[i]]$result)) {
              adalasso.final[[i]] <- adalasso.full[[i]]$result$important
       } else {
              adalasso.final[[i]] <- adalasso.error[[i]]
       }
}

#combine diagnostics
diagnostics <- data.frame(matrix(ncol = 3 , nrow = 10))
colnames(diagnostics) <- c("data.seed" , "model.seed.ridge" , "model.seed.nu")
for(i in 1:length(adalasso.final)) {
        diagnostics[i , "data.seed"] <- adalasso.final[[i]]$diagnostics$data.seed
        diagnostics[i , "model.seed.ridge"] <- adalasso.final[[i]]$diagnostics$model.seed.ridge
        diagnostics[i , "model.seed.nu"] <- adalasso.final[[i]]$diagnostics$model.seed.nu
}


#save files
saveRDS(adalasso.result , "???????adalasso_resultall.RData")
saveRDS(adalasso.error , "???????adalasso_error.RData")
saveRDS(adalasso.final , "???????adalasso_resultmain.RData")