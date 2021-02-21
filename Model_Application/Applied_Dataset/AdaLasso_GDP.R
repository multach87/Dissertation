#load libraries
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.full <- readRDS()
GDP <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/gdp_split.RData")

#lasso application function
adalasso.sim.fnct <- function(data) {
        #create simulation tracker
        #tracker <- as.vector(unlist(data$conditions)) 
        
        #print tracker of status
        cat("iteration = " , data$track , ";\n")
      
       #load X, Y, p, n
       X <- as.matrix(data$train[ , -c(1 , 15)])
       #cat("X = " , X , "\n")
       #cat("class(X) = " , class(X) , "\n")
       Y <- data$train$y.net
       #cat("class(Y) = " , class(Y) , "\n")
       n <- length(Y)
       
       #ridge coefs for weighting
       lambda.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       ridge.model <- cv.glmnet(x = X , y = Y , lambda = lambda.try , alpha = 0)
       lambda.ridge.opt <- ridge.model$lambda.min
       best.ridge.coefs <- predict(ridge.model , type = "coefficients" ,
                                   s = lambda.ridge.opt)[-1]
       #cat("post.predict.1 \n")
       ##grid of nu/gamma values to try
       nu.try <- exp(seq(log(0.01) , log(10) , length.out = 100))
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
                                     metrics_and_info = list(#model.seed.ridge = seed.ridge ,
                                                             #model.seed.prenu = seed.pre.nu , 
                                                             #model.seed.nu = seed ,
                                                             ridge.coefs = best.ridge.coefs ,
                                                             weights = 1 / abs(best.ridge.coefs)^nu.try[i] , 
                                                             nu = nu.try[i] , 
                                                             lambda = lambda.adalasso.opt , 
                                                             coefs = best.adalasso.coefs , 
                                                             mpe = adalasso.model$cvm[which(adalasso.model$lambda == lambda.adalasso.opt)] , 
                                                             mpe.sd = adalasso.model$cvsd[which(adalasso.model$lambda == lambda.adalasso.opt)])) 
                                                             #fpr = length(which(best.adalasso.coefs[c(5:p)] != 0)) / length(best.adalasso.coefs[c(5:p)]) , 
                                                             #fnr = length(which(best.adalasso.coefs[c(1:4)] == 0)) / length(best.adalasso.coefs[1:4])))
       }
       
       adalasso.nu.cv.mpe <- numeric()
       #adalasso.seeds.ridge <- numeric()
       #adalasso.seeds.prenu <- numeric()
       #adalasso.seeds.nu <- numeric()
       for(i in 1:length(adalasso.nu.cv)) {
         adalasso.nu.cv.mpe[i] <- adalasso.nu.cv[[i]]$metrics_and_info$mpe
         #adalasso.seeds.ridge[i] <- adalasso.nu.cv[[i]]$metrics_and_info$model.seed.ridge
         #adalasso.seeds.prenu[i] <- adalasso.nu.cv[[i]]$metrics_and_info$model.seed.prenu
         #adalasso.seeds.nu[i] <- adalasso.nu.cv[[i]]$metrics_and_info$model.seed.nu
       }
       #cat("which.min = " , which.min(adalasso.nu.cv.mpe) , "\n")
       adalasso.model <- adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]
       lambda.adalasso.opt <- adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$lambda
       #cat("lambda.min = " , lambda.adalasso.opt , "\n")
       adalasso.coefs <- adalasso.nu.cv[[which.min(adalasso.nu.cv.mpe)]]$metrics_and_info$coefs
       #cat("post.predict.2 \n")
       n.coefs <- sum(adalasso.coefs != 0)
       
       #specify test data
       test.X <- as.matrix(data$test[ , -c(1 , 15)])
       #cat("class(test.X) = " , class(test.X) , "\n")
       test.Y <- data$test$y.net
       
       #apply to test set
       pred.y <- test.X %*% adalasso.coefs
       #cat("pred.y = " , pred.y , "\n")
       resid <- pred.y - test.Y
       resid.sq <- resid^2
       sum.resid.sq <- sum(resid.sq)
       mse <- sum.resid.sq / n

       #initialize important info dataframe
       #put conditions, model info, and metrics into list
       return(list(model = list(full.model = adalasso.model , 
                                lambda = lambda.adalasso.opt , 
                                coefs = adalasso.coefs) , 
                   metrics = list(mse = mse , 
                                   n.coefs = n.coefs
                                  )
                   )
       )

}




#run across full dataset
adalasso.full <- GDP %>%   
       map(safely(adalasso.sim.fnct))

saveRDS(adalasso.full , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Applied_Storage/adalasso_GDP.RData")


