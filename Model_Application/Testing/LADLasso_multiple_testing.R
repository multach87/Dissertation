#load libraries
library(quantreg)
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.full <- readRDS()
debug.data <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Dissertation_Git/Data_Generation/Data_Storage/testing10_data_091520.RData")

#LAD lasso function with two-way CV for selecting both lambda and nu/gamma
ladlasso.sim.fnct <- function(data) {
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
       seed.ridge <- sample(sample(x = c(1:1000000000) , size = 10 , replace = FALSE) , size = 1)
       set.seed(seed.ridge)
       n <- length(y)
       lambda.try <- seq(log(0.01) , log(1400) , length.out = 100)
       lambda.try <- exp(lambda.try)
       nu.try <- exp(seq(log(0.01) , log(10) , length.out = 100))
       seed.nu <- rnorm(length(nu.try))
       #ridge coefs for weighting
       lambda.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       ridge.model <- cv.glmnet(x = X , y = Y , lambda = lambda.try , alpha = 0)
       lambda.ridge.opt <- ridge.model$lambda.min
       best.ridge.coefs <- predict(ridge.model , type = "coefficients" ,
                                   s = lambda.ridge.opt)[-1]
       ##initialize list of best ladlasso results from each nu/gamma
       ladlasso.nu.cv <- list()
       for(i in 1:length(nu.try)) {
              seed <- seed.nu[i]
              set.seed(seed)
              weights <- 1 / (abs(best.ridge.coefs)^nu.try[i])
              BIC <- rep(0 , 100)
              for (k in 1:100){
                     rqfit <- rq.fit.lasso(x , y , lambda = lambda.try[k] * weights)
                     betalad_tmp <- rqfit$coef
                     betalad_tmp <- betalad_tmp * (betalad_tmp > 1e-8)
                     mse <- mean(abs(rqfit$resi))
                     mdsize <- length(which(betalad_tmp != 0))
                     BIC[k] <- log(mse) + mdsize * log(n) / n
              }
              step <- which.min(BIC) 
              #cat("step/BIC min = " , step , "\n")
              betalad <- rq.fit.lasso(x , y , lambda = lambda.try[step] * weights)$coef
              ladlasso <- betalad * (betalad > 1e-8)
              lambda.ladlasso.opt <- lambda.try[step]
              
              #coeff.lad <- c(adalass$intercept , ladlasso)
              coeff2.lad <- ladlasso              # get rid of intercept
              pred.lad <- x %*% coeff2.lad #+ coeff.lad[1]
              st.lad <- sum(coeff2.lad != 0)                                          # number nonzero
              mse.lad <- sum((y - pred.lad) ^ 2) / (n - st.lad - 1)
              sd.mse.lad <- sd((y - pred.lad) ^ 2 / (n - st.lad - 1))
              #save list of all info from best model
              ladlasso.nu.cv[[i]] <- list(other.info = list(fit = pred.lad , 
                                                            st = st.lad) , 
                                          metrics_and_info = list(BIC.min = min(BIC) , 
                                                                  which.BIC.min = step , 
                                                                  model.seed.ridge = seed.ridge ,
                                                                  model.seed.nu = seed ,
                                                                  ridge.coefs = best.ridge.coefs ,
                                                                  weights = weights , 
                                                                  nu = nu.try[i] , 
                                                                  lambda = lambda.try[step] , 
                                                                  coefs = coeff2.lad , 
                                                                  mpe = mse.lad , 
                                                                  mpe.sd = sd.mse.lad ,
                                                                  fpr = length(which(coeff2.lad[c(5:p)] != 0)) / length(coeff2.lad[c(5:p)]) , 
                                                                  fnr = length(which(coeff2.lad[c(1:4)] == 0)) / length(coeff2.lad[1:4])))
              
       }
       #find/store minimizing nu/gamma, seeds, minimized BIC/step
       ladlasso.nu.cv.mpe <- numeric()
       ladlasso.seeds.ridge <- numeric()
       ladlasso.seeds.nu <- numeric()
       ladlasso.BIC.mins <- numeric()
       ladlasso.which.BIC.mins <- numeric()
       for(i in 1:length(ladlasso.nu.cv)) {
              ladlasso.nu.cv.mpe[i] <- ladlasso.nu.cv[[i]]$metrics_and_info$mpe
              ladlasso.seeds.ridge[i] <- ladlasso.nu.cv[[i]]$metrics_and_info$model.seed.ridge
              ladlasso.seeds.nu[i] <- ladlasso.nu.cv[[i]]$metrics_and_info$model.seed.nu
              ladlasso.BIC.mins[i] <- ladlasso.nu.cv[[i]]$metrics_and_info$BIC.min
              ladlasso.which.BIC.mins[i] <- ladlasso.nu.cv[[i]]$metrics_and_info$which.BIC.min
       }
       #store BEST ladlasso result plus all seeds
       ###below is used to check that seeds are regenerated properly and not uniform
       return(list(BICs = ladlasso.BIC.mins , 
                   which.BICs = ladlasso.which.BIC.mins ,
                   mpes = ladlasso.nu.cv.mpe , 
                   seeds.ridge = ladlasso.seeds.ridge , 
                   seeds.nu = ladlasso.seeds.nu ,  
                   model = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]] ,
                   important = list(diagnostics = data.frame(cbind(data.seed = tracker[7] ,
                                                                   model.seed.ridge = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge , 
                                                                   model.seed.nu = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$model.seed.nu)) , 
                                    coefs = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$coefs , 
                                    weights = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$weights ,
                                    info = data.frame(cbind(n = tracker[1] , 
                                                            p = tracker[2] , 
                                                            eta.x = tracker[3] , 
                                                            eta.y = tracker[4] , 
                                                            g = tracker[5] , 
                                                            h = tracker[6] , 
                                                            lambda = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$lambda ,
                                                            nu = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$nu ,
                                                            mpe = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$mpe , 
                                                            mpe.sd = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$mpe.sd , 
                                                            fpr = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$fpr , 
                                                            fnr = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$fnr
                                    )
                                    )
                   )
       )
       )
}

#run across full dataset
ladlasso.full <- debug.data %>%   
       map(safely(ladlasso.sim.fnct))

#dealing with error/result from map(safely())
#create empty lists for error + result
ladlasso.error <- list()
ladlasso.result <- list()
ladlasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(ladlasso.full)) { 
       #iteration tracker
       cat("i = " , i , "\n")
       #fill error list
       ladlasso.error[[i]] <- list(error = ladlasso.full[[i]]$error , 
                                   condition = as.data.frame(unlist(debug.data[[i]]$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
       #fill in results if results aren't NULL from safely()
       ladlasso.result[[i]] <- ladlasso.full[[i]]$result
       #fill final list
       if(!is.null(ladlasso.full[[i]]$result)) {
              ladlasso.final[[i]] <- ladlasso.full[[i]]$result$important
       } else {
              ladlasso.final[[i]] <- ladlasso.error[[i]]
       }
}

#save files
saveRDS(ladlasso.result , "???????ladlasso_resultall.RData")
saveRDS(ladlasso.error , "???????ladlasso_error.RData")
saveRDS(ladlasso.final , "???????ladlasso_resultmain.RData")