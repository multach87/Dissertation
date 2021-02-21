#load libraries
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.full <- readRDS()
ribo <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/ribo_split.RData")

#elnet5 application function
adaelnet5.sim.fnct <- function(data) {
        #create simulation tracker
        #tracker <- as.vector(unlist(data$conditions)) 
        
        #print tracker of status
        cat("iteration = " , data$track , ";\n")
      
       #load X, Y, p, n
       X <- as.matrix(data$train[ , -1])
       #cat("X = " , X , "\n")
       #cat("class(X) = " , class(X) , "\n")
       Y <- data$train[ , 1]
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
       ##initialize list of best adaelnet5 results from each nu/gamma
       adaelnet5.nu.cv <- list()
       for(i in 1:length(nu.try)) {
         #seed <- seed.nu[i]
         #set.seed(seed)
         #single adaptive elnet5 run with ridge weighting and nu = 1
         adaelnet5.model <- cv.glmnet(X , Y , family = "gaussian" ,
                                     lambda = lambda.try , 
                                     penalty.factor = 1 / abs(best.ridge.coefs)^nu.try[i] , alpha = 0.5)
         lambda.adaelnet5.opt <- adaelnet5.model$lambda.min
         best.adaelnet5.coefs <- predict(adaelnet5.model , type = "coefficients" ,
                                        s = lambda.adaelnet5.opt)[-1]
         adaelnet5.nu.cv[[i]] <- list(model = list(full.model = adaelnet5.model , 
                                                  lambda = lambda.adaelnet5.opt , 
                                                  coefs = best.adaelnet5.coefs) , 
                                     metrics_and_info = list(#model.seed.ridge = seed.ridge ,
                                                             #model.seed.prenu = seed.pre.nu , 
                                                             #model.seed.nu = seed ,
                                                             ridge.coefs = best.ridge.coefs ,
                                                             weights = 1 / abs(best.ridge.coefs)^nu.try[i] , 
                                                             nu = nu.try[i] , 
                                                             lambda = lambda.adaelnet5.opt , 
                                                             coefs = best.adaelnet5.coefs , 
                                                             mpe = adaelnet5.model$cvm[which(adaelnet5.model$lambda == lambda.adaelnet5.opt)] , 
                                                             mpe.sd = adaelnet5.model$cvsd[which(adaelnet5.model$lambda == lambda.adaelnet5.opt)])) 
                                                             #fpr = length(which(best.adaelnet5.coefs[c(5:p)] != 0)) / length(best.adaelnet5.coefs[c(5:p)]) , 
                                                             #fnr = length(which(best.adaelnet5.coefs[c(1:4)] == 0)) / length(best.adaelnet5.coefs[1:4])))
       }
       
       adaelnet5.nu.cv.mpe <- numeric()
       #adaelnet5.seeds.ridge <- numeric()
       #adaelnet5.seeds.prenu <- numeric()
       #adaelnet5.seeds.nu <- numeric()
       for(i in 1:length(adaelnet5.nu.cv)) {
         adaelnet5.nu.cv.mpe[i] <- adaelnet5.nu.cv[[i]]$metrics_and_info$mpe
         #adaelnet5.seeds.ridge[i] <- adaelnet5.nu.cv[[i]]$metrics_and_info$model.seed.ridge
         #adaelnet5.seeds.prenu[i] <- adaelnet5.nu.cv[[i]]$metrics_and_info$model.seed.prenu
         #adaelnet5.seeds.nu[i] <- adaelnet5.nu.cv[[i]]$metrics_and_info$model.seed.nu
       }
       #cat("which.min = " , which.min(adaelnet5.nu.cv.mpe) , "\n")
       adaelnet5.model <- adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]
       lambda.adaelnet5.opt <- adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$lambda
       #cat("lambda.min = " , lambda.adaelnet5.opt , "\n")
       adaelnet5.coefs <- adaelnet5.nu.cv[[which.min(adaelnet5.nu.cv.mpe)]]$metrics_and_info$coefs
       #cat("post.predict.2 \n")
       n.coefs <- sum(adaelnet5.coefs != 0)
       
       #specify test data
       test.X <- as.matrix(data$test[ , -1])
       #cat("class(test.X) = " , class(test.X) , "\n")
       test.Y <- data$test[ , 1]
       
       #apply to test set
       pred.y <- test.X %*% adaelnet5.coefs
       #cat("pred.y = " , pred.y , "\n")
       resid <- pred.y - test.Y
       resid.sq <- resid^2
       sum.resid.sq <- sum(resid.sq)
       mse <- sum.resid.sq / n

       #initialize important info dataframe
       #put conditions, model info, and metrics into list
       return(list(model = list(full.model = adaelnet5.model , 
                                lambda = lambda.adaelnet5.opt , 
                                coefs = adaelnet5.coefs) , 
                   metrics = list(mse = mse , 
                                   n.coefs = n.coefs
                                  )
                   )
       )

}




#run across full dataset
adaelnet5.full <- ribo %>%   
       map(safely(adaelnet5.sim.fnct))

saveRDS(adaelnet5.full , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Applied_Storage/adaelnet5_ribo.RData")


