#package libraries
library(dplyr)

#read in files
SNCDladelnet5.outlier25 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADelnet5_outlier25_500_COEFS.RData")
SNCDladelnet5.outlier50 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADelnet5_outlier50_500_COEFS.RData")
SNCDladelnet5.outlier100 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADelnet5_outlier100_500_COEFS.RData")
SNCDladelnet5.outlier200 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADelnet5_outlier200_500_COEFS.RData")

SNCDladelnet5.distr25 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADelnet5_distr25_500_COEFS.RData")
SNCDladelnet5.distr50 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADelnet5_distr50_500_COEFS.RData")
SNCDladelnet5.distr100 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADelnet5_distr100_500_COEFS.RData")
SNCDladelnet5.distr200 <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADelnet5_distr200_500_COEFS.RData")


#combine datasets
SNCDladelnet5.all <- c(SNCDladelnet5.outlier25 , SNCDladelnet5.outlier50 , 
                      SNCDladelnet5.outlier100 , SNCDladelnet5.outlier200 , 
                      SNCDladelnet5.distr25 , SNCDladelnet5.distr50 , 
                      SNCDladelnet5.distr100 , SNCDladelnet5.distr200)

SNCDladelnet5.error <- list()
SNCDladelnet5.result <- list()
SNCDladelnet5.final <- list()
#split data into separate error and result lists
for(i in 1:length(SNCDladelnet5.all)) { 
  #iteration tracker
  #cat("i = " , i , "\n")
  #fill error list
  SNCDladelnet5.error[[i]] <- list(error = SNCDladelnet5.all[[i]]$error , 
                                  condition = as.data.frame(unlist(SNCDladelnet5.all[[i]]$result$condition) , 
                                                            n = n , p = p , 
                                                            eta.x = eta.x , eta.y = eta.y , 
                                                            g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  SNCDladelnet5.result[[i]] <- SNCDladelnet5.all[[i]]$result
  #fill final list
  if(!is.null(SNCDladelnet5.all[[i]]$result)) {
    SNCDladelnet5.final[[i]] <- SNCDladelnet5.all[[i]]$result$important
  } else {
    cat("error at i = " , i , "/n")
    SNCDladelnet5.final[[i]] <- SNCDladelnet5.error[[i]]
  }
}

#initialize dataframe
SNCDladelnet5.maindf <- data.frame(matrix(ncol = ncol(SNCDladelnet5.final[[1]])))
colnames(SNCDladelnet5.maindf) <- colnames(SNCDladelnet5.final[[1]])

#initialize error vector
SNCDladelnet5.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(SNCDladelnet5.final)) {
  if(is.null(SNCDladelnet5.final[[i]]$error)) {
    SNCDladelnet5.maindf[i , ] <- SNCDladelnet5.final[[i]]
  } else {
    cat("error at i = " , i , "\n")
    SNCDladelnet5.maindf[i , 1:7] <- HD.data.current[[i]]$conditions
    SNCDladelnet5.errors <- c(SNCDladelnet5.errors , i)
    #SNCDladelnet5.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
    #SNCDladelnet5.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  }
}





#Check results
mean(SNCDladelnet5.maindf[ , "fpr"])
mean(SNCDladelnet5.maindf[ , "fpr"] , na.rm = T) #with null models included
mean(SNCDladelnet5.maindf[ , "fnr"])
mean(SNCDladelnet5.maindf[ , "fnr"] , na.rm = T) #with null models included


saveRDS(SNCDladelnet5.result , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/sncdladelnet5_result_500_COEFS.RData")
saveRDS(SNCDladelnet5.error , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/sncdladelnet5_error_500_COEFS.RData")
saveRDS(SNCDladelnet5.final , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/sncdladelnet5_resultmain_500_COEFS.RData")


saveRDS(SNCDladelnet5.maindf , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/sncdladelnet5_resultDF_500_COEFS.RData")
saveRDS(SNCDladelnet5.errors , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/sncdladelnet5_errorindices_500_COEFS.RData")
