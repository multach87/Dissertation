#load libraries
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.full <- readRDS()
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")


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
lasso.debug <- debug.data %>%   
       map(safely(lasso.sim.fnct))

#saveRDS(lasso.debug , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/lasso_debug.RData")
#dealing with error/result from map(safely())
#create empty lists for error + result
lasso.error <- list()
lasso.result <- list()
lasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(lasso.debug)) { 
       #iteration tracker
       cat("i = " , i , "\n")
       #fill error list
       cat("pre.error \n")
       lasso.error[[i]] <- list(error = lasso.debug[[i]]$error , 
                              condition = as.data.frame(unlist(debug.data[[i]]$condition) ,
                                                        n = n , 
                                                        p = p , 
                                                        eta.x = eta.x , 
                                                        eta.y = eta.y , 
                                                        g = g , 
                                                        h = h , 
                                                        seed = seed))
       #fill in results if results aren't NULL from safely()
       cat("pre.result \n")
       lasso.result[[i]] <- lasso.debug[[i]]$`result`
       #fill final list
       cat("pre.final \n")
       if(!is.null(lasso.debug[[i]]$`result`)) {
              lasso.final[[i]] <- lasso.debug[[i]]$`result`$important
       } else {
              lasso.final[[i]] <- lasso.error[[i]]
       }
}

diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(debug.data)))
colnames(diagnostics) <- c("data.seed" , "model.seed")
for(i in 1:length(lasso.final)) {
        diagnostics[i , "data.seed"] <- lasso.final[[i]]$diagnostics$data.seed
        diagnostics[i , "model.seed"] <- lasso.final[[i]]$diagnostics$model.seed
}

cat("Hello world , \n")

#save files
saveRDS(lasso.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/lasso_result_DEBUG.RData")
saveRDS(lasso.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/lasso_error_DEBUG2.RData")
saveRDS(lasso.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/lasso_resultmain_DEBUG.RData")
saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/lasso_diagnostics_DEBUG.RData")


