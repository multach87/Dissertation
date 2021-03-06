#load libraries
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.full <- readRDS()
#debug.data <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
testing10.data <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Dissertation_Git/Data_Generation/Data_Storage/testing10_data_091720.RData")


#lasso application function
lasso.sim.fnct <- function(data) {
        #create simulation tracker
        tracker <- as.vector(unlist(data$conditions)) 
        
        #print tracker of status
        cat("n = " , tracker[1] , " , p = " , tracker[2] ,
            " , eta.x = " , tracker[3] , " , eta.y = " , tracker[4] ,
            " , g = " , tracker[5] , " , h = " , tracker[6] ,
            ";\n")
      
       #load X, Y, p, n
       X <- data$X
       Y <- data$Y
       p <- data$conditions$p
       n <- length(Y)
       
       #set seed for generating ridge coefficients for weighting
       seed.model <- data$seeds[ , "seed.6"]
       set.seed(seed.model)
       
       #set grid of lambda values
       lambda.lasso.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       #lasso model
       lasso.model <- cv.glmnet(X , Y , family = "gaussian" ,
                                lambda = lambda.lasso.try)
       lambda.lasso.opt <- lasso.model$lambda.min
       lasso.coefs <- predict(lasso.model , type = "coefficients" ,
                              s = lambda.lasso.opt)[-1]
       #initialize important info dataframe
       #put conditions, model info, and metrics into list
       return(list(seed.model = seed.model ,  
                   model = list(full.model = lasso.model , 
                                lambda = lambda.lasso.opt , coefs = lasso.coefs) , 
                   metrics = list(mpe = lasso.model$cvm[which(lasso.model$lambda == lambda.lasso.opt)] , 
                                   mpe.sd = lasso.model$cvsd[which(lasso.model$lambda == lambda.lasso.opt)] , 
                                   fpr = length(which(lasso.coefs[c(5:p)] != 0)) / length(lasso.coefs[c(5:p)]) , 
                                   fnr = length(which(lasso.coefs[c(1:4)] == 0)) / length(lasso.coefs[1:4])) , 
                   important = list(diagnostics = data.frame(cbind(data.seed = tracker[7] ,
                                                                   model.seed = seed.model)) , 
                                    coefs = lasso.coefs , 
                                    info = data.frame(cbind(n = tracker[1] , 
                                                            p = tracker[2] , 
                                                            eta.x = tracker[3] , 
                                                            eta.y = tracker[4] , 
                                                            g = tracker[5] , 
                                                            h = tracker[6] , 
                                                            data.seed = tracker[7] ,
                                                            model.seed = seed.model , 
                                                            lambda = lambda.lasso.opt , 
                                                            mpe = lasso.model$cvm[which(lasso.model$lambda == lambda.lasso.opt)] , 
                                                            mpe.sd = lasso.model$cvsd[which(lasso.model$lambda == lambda.lasso.opt)] , 
                                                            fpr = length(which(lasso.coefs[c(5:p)] != 0)) / length(lasso.coefs[c(5:p)]) , 
                                                            fnr = length(which(lasso.coefs[c(1:4)] == 0)) / length(lasso.coefs[1:4])
                                                            )
                                                      )
                                    )
                                    )
                   )                                 

}




#run across full dataset
lasso.testing10 <- testing10.data %>%   
       map(safely(lasso.sim.fnct))

#dealing with error/result from map(safely())
#create empty lists for error + result
lasso.error <- list()
lasso.result <- list()
lasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(lasso.testing10)) { 
       #iteration tracker
       cat("i = " , i , "\n")
       #fill error list
       lasso.error[[i]] <- list(error = lasso.testing10[[i]]$error , 
                              condition = as.data.frame(unlist(testing10.data[[i]]$condition) ,
                                                        n = n , 
                                                        p = p , 
                                                        eta.x = eta.x , 
                                                        eta.y = eta.y , 
                                                        g = g , 
                                                        h = h , 
                                                        seed = seed))
       #fill in results if results aren't NULL from safely()
       lasso.result[[i]] <- lasso.testing10[[i]]$result
       #fill final list
       if(!is.null(lasso.testing10[[i]]$result)) {
              lasso.final[[i]] <- lasso.testing10[[i]]$result$important
       } else {
              lasso.final[[i]] <- lasso.error[[i]]
       }
}

diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(testing10.data)))
colnames(diagnostics) <- c("data.seed" , "model.seed")
for(i in 1:length(lasso.final)) {
        diagnostics[i , "data.seed"] <- lasso.final[[i]]$diagnostics$data.seed
        diagnostics[i , "model.seed"] <- lasso.final[[i]]$diagnostics$model.seed
}


#save files
saveRDS(lasso.result , "???????lasso_resultall.RData")
saveRDS(lasso.error , "???????lasso_error.RData")
saveRDS(lasso.final , "???????lasso_resultmain.RData")