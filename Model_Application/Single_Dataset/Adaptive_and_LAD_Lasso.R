#load libraries
library(quantreg)

#load data
#data.full <- readRDS()
single.data <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Dissertation_Git/Data_Generation/Data_Storage/single_data_091520.RData")
X <- single.data[[1]]$X
Y <- single.data[[1]]$Y
p <- single.data[[1]]$conditions$p

#adaptive lasso function with two-way CV for selecting both lambda and nu/gamma
adalasso.sim.fnct <- function(x , y , p) { 
       seed.ridge <- sample(x = c(1:1000) , size = 1 , replace = FALSE)
       set.seed(seed.ridge)
       #ridge coefs for weighting
       lambda.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       ridge.model <- cv.glmnet(x = X , y = Y , lambda = lambda.try , alpha = 0)
       lambda.ridge.opt <- ridge.model$lambda.min
       best.ridge.coefs <- predict(ridge.model , type = "coefficients" ,
                                   s = lambda.ridge.opt)[-1]
       ##grid of nu/gamma values to try
       nu.try <- exp(seq(log(0.01) , log(10) , length.out = 100))
       seed.nu <- rnorm(length(nu.try))
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
       return(list(mpes = adalasso.nu.cv.mpe , seeds.ridge = adalass.seeds.ridge , 
                   seeds.nu = adalass.seeds.nu ,  
                   model = adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]))
}
ladlasso.fnct.test <- function(x , y) {
       seed.ridge <- sample(x = c(1:1000) , size = 1 , replace = FALSE)
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
       ##initialize list of best adalasso results from each nu/gamma
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
       #store BEST adalasso result plus all seeds
       ###below is used to check that seeds are regenerated properly and not uniform
       return(list(BICs = ladlasso.BIC.mins , which.BICs = ladlasso.which.BIC.mins ,
                   mpes = ladlasso.nu.cv.mpe , seeds.ridge = ladlasso.seeds.ridge , 
                   seeds.nu = ladlasso.seeds.nu ,  
                   model = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]))
}

