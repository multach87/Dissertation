#libraries
library(mvtnorm)
library(magrittr)
library(purrr)

#load test set
test500.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/testset_500_021721.RData")

#load model data
#load data
msaelnet5.outlier25 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_outlier25_500.RData")
msaelnet5.outlier50 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_outlier50_500.RData")
msaelnet5.outlier100 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_outlier100_500.RData")
msaelnet5.outlier200 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_outlier200_500.RData")
msaelnet5.distr25 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_distr25_500.RData")
msaelnet5.distr50 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_distr50_500.RData")
msaelnet5.distr100 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_distr100_500.RData")
msaelnet5.distr200 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_distr200_500.RData")

#combine into one list
msaelnet5.500.data <- c(msaelnet5.outlier25 , msaelnet5.outlier50 , 
                   msaelnet5.outlier100 , msaelnet5.outlier200 , 
                   msaelnet5.distr25 , msaelnet5.distr50 , 
                   msaelnet5.distr100 , msaelnet5.distr200)

#clear split-by-n data from environment
rm(list = c("msaelnet5.distr25" , "msaelnet5.distr50" , 
            "msaelnet5.distr100" , "msaelnet5.distr200" ,
            "msaelnet5.outlier25" , "msaelnet5.outlier50" , 
            "msaelnet5.outlier100" , "msaelnet5.outlier200"))


combined.data <- list()
#combine data
for(i in 1:length(msaelnet5.500.data)) {
  combined.data[[i]] <- c(msaelnet5.500.data[[i]] , test500.data[[i]])
}

#clear separate data so memory isn't taken up
rm(list = c("msaelnet5.500.data" , "test500.data"))

msebias.msaelnet5 <- function(data) {
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
msaelnet5.mse.bias.HALF <- combined.data %>%   
  map(safely(msebias.msaelnet5))

saveRDS(msaelnet5.mse.bias.HALF , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/mse_bias/msebias_msaelnet5_500.RData")
