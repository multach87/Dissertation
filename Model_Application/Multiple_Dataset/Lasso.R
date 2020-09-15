#load libraries
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.full <- readRDS()
debug.data <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Dissertation_Git/Data_Generation/Data_Storage/testing10_data_091520.RData")
test.data <- debug.data[[1]]


lasso.info <- function(data) {
       seed <- sample(x = c(1:1000) , size = 1 , replace = FALSE)
       set.seed(seed)
       #create vector of data conditions
       tracker <- as.vector(unlist(data$conditions)) 
       #print tracker of status
       cat("n = " , tracker[1] , " , p = " , tracker[2] ,
           " , eta.x = " , tracker[3] , " , eta.y = " , tracker[4] ,
           " , g = " , tracker[5] , " , h = " , tracker[6] ,
           ";\n")
       #set X, Y, p
       X <- data$X
       Y <- data$Y
       p <- data$conditions$p
       #set grid of lambda values
       lambda.lasso.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       #lasso model
       lasso.model <- cv.glmnet(X , Y , family = "gaussian" ,
                                lambda = lambda.lasso.try)
       lambda.lasso.opt <- lasso.model$lambda.min
       lasso.coefs <- predict(lasso.model , type = "coefficients" ,
                              s = lambda.lasso.opt)[2 : (p + 1)]
       #initialize important info dataframe
       #put conditions, model info, and metrics into list
       return(list(condition = as.data.frame(unlist(data$condition) , n = n , p = p , 
                                             eta.x = eta.x , eta.y = eta.y , 
                                             g = g , h = h , seed = seed) , 
                   model = list(full.model = lasso.model , 
                                lambda = lambda.lasso.opt , coefs = lasso.coefs) , 
                   metrics = list(mpe = lasso.model$cvm[which(lasso.model$lambda == lambda.lasso.opt)] , 
                                   mpe.sd = lasso.model$cvsd[which(lasso.model$lambda == lambda.lasso.opt)] , 
                                   fpr = length(which(lasso.coefs[c(5:p)] != 0)) / length(lasso.coefs[c(5:p)]) , 
                                   fnr = length(which(lasso.coefs[c(1:4)] == 0)) / length(lasso.coefs[1:4])) , 
                   important = list(coefs = lasso.coefs , info = data.frame(cbind(n = tracker[1] , p = tracker[2] , 
                                             eta.x = tracker[3] , eta.y = tracker[4] , 
                                             g = tracker[5] , h = tracker[6] , data.seed = tracker[7] ,
                                             model.seed = seed , lambda = lambda.lasso.opt , 
                                             mpe = lasso.model$cvm[which(lasso.model$lambda == lambda.lasso.opt)] , 
                                             mpe.sd = lasso.model$cvsd[which(lasso.model$lambda == lambda.lasso.opt)] , 
                                             fpr = length(which(lasso.coefs[c(5:p)] != 0)) / length(lasso.coefs[c(5:p)]) , 
                                             fnr = length(which(lasso.coefs[c(1:4)] == 0)) / length(lasso.coefs[1:4]))))))                                   

}

lasso.info(test.data)


#run across full dataset
lasso.full <- debug.data %>%   
       map(safely(lasso.info))

#dealing with error/result from map(safely())
#create empty lists for error + result
lasso.error <- list()
lasso.result <- list()
lasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(lasso.full)) { 
       #iteration tracker
       cat("i = " , i , "\n")
       #fill error list
       lasso.error[[i]] <- list(error = lasso.full[[i]]$error , 
                              condition = as.data.frame(unlist(debug.data[[i]]$condition) , n = n , p = p , 
                                                        eta.x = eta.x , eta.y = eta.y , 
                                                        g = g , h = h , seed = seed))
       #fill in results if results aren't NULL from safely()
       lasso.result[[i]] <- lasso.full[[i]]$result
       #fill final list
       if(!is.null(lasso.full[[i]]$result)) {
              lasso.final[[i]] <- lasso.full[[i]]$result$important
       } else {
              lasso.final[[i]] <- lasso.error[[i]]
       }
}











#testing error from map(safely()) and loop - SEEMS TO WORK
debug.data2 <- debug.data
debug.data2[[10]] <- list(conditions = debug.data2[[10]]$conditions)
#run across full dataset
lasso.full <- debug.data2 %>%   
       map(safely(lasso.info))

#dealing with error/result from map(safely())
#create empty lists for error + result
lasso.error <- list()
lasso.result <- list()
lasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(lasso.full)) { 
       #iteration tracker
       cat("i = " , i , "\n")
       #fill error list
       lasso.error[[i]] <- list(error = lasso.full[[i]]$error , 
                                condition = as.data.frame(unlist(debug.data[[i]]$condition) , n = n , p = p , 
                                                          eta.x = eta.x , eta.y = eta.y , 
                                                          g = g , h = h , seed = seed))
       #fill in results if results aren't NULL from safely()
       lasso.result[[i]] <- lasso.full[[i]]$result
       #fill final list
       if(!is.null(lasso.full[[i]]$result)) {
              lasso.final[[i]] <- lasso.full[[i]]$result$important
       } else {
              lasso.final[[i]] <- lasso.error[[i]]
       }
}






#testing stuff
test.data <- debug.data[[1]]
X <- test.data$X
Y <- test.data$Y
p <- test.data$conditions$p


tracker <- as.vector(unlist(test.data$conditions))
tracker
cat("n = " , tracker[1] , " , p = " , tracker[2] ,
    " , eta.x = " , tracker[3] , " , eta.y = " , tracker[4] ,
    " , g = " , tracker[5] , " , h = " , tracker[6] ,
    ";\n")  
test.data$conditions



#lambda.lasso.try = seq(0.01,0.6,length.out=100)
lambda.lasso.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
lasso.model <- cv.glmnet(X , Y , family = "gaussian" ,
                         lambda = lambda.lasso.try)
lambda.lasso.opt <- lasso.model$lambda.min
lasso.coefs <- predict(lasso.model , type = "coefficients" ,
                       s = lambda.lasso.opt)[2 : (p + 1)]
model <- list(full.model = lasso.model , lambda = lambda.lasso.opt , coefs = lasso.coefs)

#Metrics
which(lasso.model$lambda == lambda.lasso.opt)
lasso.err <- list(mpe = lasso.model$cvm[which(lasso.model$lambda == lambda.lasso.opt)] , 
                  mpe.sd = lasso.model$cvsd[which(lasso.model$lambda == lambda.lasso.opt)])
lasso.err
metrics <- list(mpe = lasso.model$cvm[which(lasso.model$lambda == lambda.lasso.opt)] , 
                mpe.sd = lasso.model$cvsd[which(lasso.model$lambda == lambda.lasso.opt)] , 
                fpr = length(which(lasso.coefs[c(5:p)] != 0)) / length(lasso.coefs[c(5:p)]) , 
                fnr = length(which(lasso.coefs[c(1:4)] == 0)) / length(lasso.coefs[1:4]))





#
