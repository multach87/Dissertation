#load libraries
library(glmnet)
library(magrittr)
library(purrr)
library(msaenet)

#load data
#data.full <- readRDS()
GDP <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/gdp_split.RData")

#elnet5 application function
msadaelnet5.sim.fnct <- function(data) {
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
       nu.try <- exp(seq(log(0.01) , log(10) , length.out = 100))
       #seed.pre.nu <- data$seeds[ , "seed.15"]
       #set.seed(seed.pre.nu)
       #seed.nu <- sample(rnorm(n = 1000000000) , size = length(nu.try) , replace = FALSE)
       ##initialize list of best msadaelnet5 results from each nu/gamma
       msadaelnet5.nu.cv <- list()
       for(i in 1:length(nu.try)) {
         #seed <- seed.nu[i]
         #set.seed(seed)
         #single adaptive lasso run with ridge weighting and nu = 1
         msadaelnet5.model <- msaenet(x = X , y = Y , family = "gaussian" , 
                                    init = "ridge" , alphas = 0.5 , 
                                    tune = "cv" , nfolds = 5L , 
                                    rule = "lambda.min" , nsteps = 10L , 
                                    tune.nsteps = "max" , scale = nu.try[i])
         lambda.msadaelnet5.opt <- msadaelnet5.model[["best.lambdas"]][[11]]
         best.msadaelnet5.coefs <- coef(msadaelnet5.model) #coefficients
         msadaelnet5.nu.cv[[i]] <- list(model = list(full.model = msadaelnet5.model , 
                                                   lambda = lambda.msadaelnet5.opt , 
                                                   coefs = best.msadaelnet5.coefs) , 
                                      metrics_and_info = list(#model.seed.ridge = seed.ridge ,
                                        #model.seed.prenu = seed.pre.nu , 
                                        #model.seed.nu = seed ,
                                        weights = msadaelnet5.model[["adapen.list"]][[10]] , 
                                        nu = nu.try[i] , 
                                        lambda = lambda.msadaelnet5.opt , 
                                        coefs = best.msadaelnet5.coefs , 
                                        mpe = msadaelnet5.model[["step.criterion"]][[11]]))
                                        #mpe.sd = msadaelnet5.model$cvsd[which(msadaelnet5.model$lambda == lambda.msadaelnet5.opt)] , 
                                        #fpr = length(which(best.msadaelnet5.coefs[c(5:p)] != 0)) / length(best.msadaelnet5.coefs[c(5:p)]) , 
                                        #fnr = length(which(best.msadaelnet5.coefs[c(1:4)] == 0)) / length(best.msadaelnet5.coefs[1:4])))
       }
       #find minimizing nu/gamma
       msadaelnet5.nu.cv.mpe <- numeric()
       #msadaelnet5.seeds.ridge <- numeric()
       #msadaelnet5.seeds.prenu <- numeric()
       #msadaelnet5.seeds.nu <- numeric()
       for(i in 1:length(msadaelnet5.nu.cv)) {
         msadaelnet5.nu.cv.mpe[i] <- msadaelnet5.nu.cv[[i]]$metrics_and_info$mpe
         #msadaelnet5.seeds.ridge[i] <- msadaelnet5.nu.cv[[i]]$metrics_and_info$model.seed.ridge
         #msadaelnet5.seeds.prenu[i] <- msadaelnet5.nu.cv[[i]]$metrics_and_info$model.seed.prenu
         #msadaelnet5.seeds.nu[i] <- msadaelnet5.nu.cv[[i]]$metrics_and_info$model.seed.nu
       }
       #cat("which.min = " , which.min(msadaelnet5.nu.cv.mpe) , "\n")
       msadaelnet5.model <- msadaelnet5.nu.cv[[which.min(msadaelnet5.nu.cv.mpe)]]
       lambda.msadaelnet5.opt <- msadaelnet5.nu.cv[[which.min(msadaelnet5.nu.cv.mpe)]]$metrics_and_info$lambda
       #cat("lambda.min = " , lambda.msadaelnet5.opt , "\n")
       msadaelnet5.coefs <- msadaelnet5.nu.cv[[which.min(msadaelnet5.nu.cv.mpe)]]$metrics_and_info$coefs
       #cat("post.predict.2 \n")
       n.coefs <- sum(msadaelnet5.coefs != 0)
       
       #specify test data
       test.X <- as.matrix(data$test[ , -c(1 , 15)])
       #cat("class(test.X) = " , class(test.X) , "\n")
       test.Y <- data$test$y.net
       
       #apply to test set
       pred.y <- test.X %*% msadaelnet5.coefs
       #cat("pred.y = " , pred.y , "\n")
       resid <- pred.y - test.Y
       resid.sq <- resid^2
       sum.resid.sq <- sum(resid.sq)
       mse <- sum.resid.sq / n

       #initialize important info dataframe
       #put conditions, model info, and metrics into list
       return(list(model = list(full.model = msadaelnet5.model , 
                                lambda = lambda.msadaelnet5.opt , 
                                coefs = msadaelnet5.coefs) , 
                   metrics = list(mse = mse , 
                                   n.coefs = n.coefs
                                  )
                   )
       )

}




#run across full dataset
msadaelnet5.full <- GDP %>%   
       map(safely(msadaelnet5.sim.fnct))

saveRDS(msadaelnet5.full , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Applied_Storage/msadaelnet5_GDP.RData")


