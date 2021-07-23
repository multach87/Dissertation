#package libraries
library(dplyr)

#read in files
SNCDladlasso.outlier25 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDladlasso_outlier25_500_COEFS.RData")
SNCDladlasso.outlier50 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDladlasso_outlier50_500_COEFS.RData")
SNCDladlasso.outlier100 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDladlasso_outlier100_500_COEFS.RData")
SNCDladlasso.outlier200 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDladlasso_outlier200_500_COEFS.RData")

SNCDladlasso.distr25 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDladlasso_distr25_500_COEFS.RData")
SNCDladlasso.distr50 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDladlasso_distr50_500_COEFS.RData")
SNCDladlasso.distr100 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDladlasso_distr100_500_COEFS.RData")
SNCDladlasso.distr200 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDladlasso_distr200_500_COEFS.RData")


#combine datasets
SNCDladLasso.all <- c(SNCDladlasso.outlier25 , SNCDladlasso.outlier50 , 
                      SNCDladlasso.outlier100 , SNCDladlasso.outlier200 , 
                      SNCDladlasso.distr25 , SNCDladlasso.distr50 , 
                      SNCDladlasso.distr100 , SNCDladlasso.distr200)

SNCDladLasso.error <- list()
SNCDladLasso.result <- list()
SNCDladLasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(SNCDladLasso.all)) { 
  #iteration tracker
  #cat("i = " , i , "\n")
  #fill error list
  SNCDladLasso.error[[i]] <- list(error = SNCDladLasso.all[[i]]$error , 
                                  condition = as.data.frame(unlist(SNCDladLasso.all[[i]]$result$condition) , 
                                                            n = n , p = p , 
                                                            eta.x = eta.x , eta.y = eta.y , 
                                                            g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  SNCDladLasso.result[[i]] <- SNCDladLasso.all[[i]]$result
  #fill final list
  if(!is.null(SNCDladLasso.all[[i]]$result)) {
    SNCDladLasso.final[[i]] <- SNCDladLasso.all[[i]]$result$important
  } else {
    cat("error at i = " , i , "/n")
    SNCDladLasso.final[[i]] <- SNCDladLasso.error[[i]]
  }
}

#initialize dataframe
SNCDladLasso.maindf <- data.frame(matrix(ncol = ncol(SNCDladLasso.final[[1]])))
colnames(SNCDladLasso.maindf) <- colnames(SNCDladLasso.final[[1]])

#initialize error vector
SNCDladLasso.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(SNCDladLasso.final)) {
  if(is.null(SNCDladLasso.final[[i]]$error)) {
    SNCDladLasso.maindf[i , ] <- SNCDladLasso.final[[i]]
  } else {
    cat("error at i = " , i , "\n")
    SNCDladLasso.maindf[i , 1:7] <- HD.data.current[[i]]$conditions
    SNCDladLasso.errors <- c(SNCDladLasso.errors , i)
    #SNCDladLasso.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
    #SNCDladLasso.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  }
}





#Check results
mean(SNCDladLasso.maindf[ , "fpr"])
mean(SNCDladLasso.maindf[ , "fpr"] , na.rm = T) #with null models included
mean(SNCDladLasso.maindf[ , "fnr"])
mean(SNCDladLasso.maindf[ , "fnr"] , na.rm = T) #with null models included


#save files
saveRDS(SNCDladLasso.result , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/sncdladlasso_result_500_COEFS.RData")
saveRDS(SNCDladLasso.error , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/sncdladlasso_error_500_COEFS.RData")
saveRDS(SNCDladLasso.final , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/sncdladlasso_resultmain_500_COEFS.RData")


saveRDS(SNCDladLasso.maindf , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/sncdladlasso_resultDF_500_COEFS.RData")
saveRDS(SNCDladLasso.errors , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/sncdladlasso_errorindices_500_COEFS.RData")


