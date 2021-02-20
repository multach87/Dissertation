#libraries
library(mvtnorm)
library(magrittr)
library(purrr)

#load test set
test500.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/testset_500_021721.RData")

#load model data
elnet5.500.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/elnet5_500.RData")

combined.data <- list()
#combine data
for(i in 1:length(elnet5.500.data)) {
  combined.data[[i]] <- c(elnet5.500.data[[i]] , test500.data[[i]])
}

#clear separate data so memory isn't taken up
rm(list = c("elnet5.500.data" , "test500.data"))

msebias.elnet5. <- function(data) {
  conditions <- as.vector(unlist(data$conditions))
  cat("n = " , conditions[1] , " , p = " , conditions[2] ,
      " , eta.x = " , conditions[3] , " , eta.y = " , conditions[4] ,
      " , g = " , conditions[5] , " , h = " , conditions[6] ,
      ";\n")
  pred.y <- data$X %*% data$result$important$coefs
  #cat("pred.y = " , pred.y , "\n")
  resid <- data$Y - pred.y
  resid.sq <- resid^2
  sum.resid.sq <- sum(resid.sq)
  mse <- sum.resid.sq / data$result$important$info$n
  
  true.coefs <- c(0.5 , 1.0 , 1.5 , 2.0)
  coefs.dif <- data$result$important$coefs[1:4] - true.coefs
  coefs.dif.sq <- coefs.dif^2
  sum.coefs.dif.sq <- sum(coefs.dif.sq)
  coefs.bias <- sum.coefs.dif.sq / 4
  cat("coefs.bias = " , coefs.bias , "\n")
  
  alpha <- data$result$important$info$alpha
  lambda <- data$result$important$info$lambda
  nu <- data$result$important$info$nu
  fpr <- data$result$important$info$fpr
  fnr <- data$result$important$info$fnr
  
  return(data.frame(cbind(n = conditions[1] ,
                          p = conditions[2] ,
                          eta.x = conditions[3] ,
                          eta.y = conditions[4] ,
                          g = conditions[5] ,
                          h = conditions[6] ,
                          data.seed = conditions[7] ,
                          alpha = alpha , 
                          lambda = lambda , 
                          nu = nu ,
                          fpr = fpr , 
                          fnr = fnr , 
                          mse = mse ,
                          coefs.bias = coefs.bias
  ) 
  ) 
  )
}

#run across full dataset
elnet5.mse.bias.HALF <- combined.data %>%   
  map(safely(msebias.elnet5.))

saveRDS(elnet5.mse.bias.HALF , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/mse_bias/msebias_elnet5_500.RData")
