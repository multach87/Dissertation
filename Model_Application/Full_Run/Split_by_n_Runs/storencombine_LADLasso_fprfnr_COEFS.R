#package libraries
library(dplyr)

#load data with fprfnr
SNCDladLasso.all <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/mse_bias/msebias_ladlasso_500_fprfnr_COEFS.RData")

SNCDladLasso.error <- list()
SNCDladLasso.result <- list()
SNCDladLasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(SNCDladLasso.all)) { 
  #iteration tracker
  #cat("i = " , i , "\n")
  #fill error list
  SNCDladLasso.error[[i]] <- list(error = SNCDladLasso.all[[i]]$error , 
                                  condition = as.data.frame(SNCDladLasso.all[[i]]$result))
  #fill in results if results aren't NULL from safely()
  SNCDladLasso.result[[i]] <- SNCDladLasso.all[[i]]$result
  #fill final list
  if(!is.null(SNCDladLasso.all[[i]]$result)) {
    SNCDladLasso.final[[i]] <- SNCDladLasso.all[[i]]$result
  } else {
    cat("error at i = " , i , "/n")
    SNCDladLasso.final[[i]] <- SNCDladLasso.error[[i]]
  }
}

#initialize dataframe
SNCDladLasso.maindf <- data.frame(matrix(ncol = ncol(SNCDladLasso.final[[1]])))
colnames(SNCDladLasso.maindf) <- colnames(SNCDladLasso.final[[1]])
colnames(SNCDladLasso.maindf)

#initialize error vector
SNCDladLasso.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(SNCDladLasso.final)) {   #length(S5NCDladLasso.final)
  if((i %% 1000) == 0) {
    cat("i = " , i , "\n")
  }
  SNCDladLasso.maindf[i , ] <- apply(SNCDladLasso.final[[i]][1 , ] , 2 , 
                                     function(x) as.numeric(as.character(x)))
  #SNCDladLasso.maindf[i , ] <- as.numeric(as.character(SNCDladLasso.final[[1]][1 , ]))
  #if(is.null(SNCDladLasso.final[[i]]$error)) {
  #  SNCDladLasso.maindf[i , ] <- SNCDladLasso.final[[i]]$result
  #} else {
  #  cat("error at i = " , i , "\n")
  #  SNCDladLasso.maindf[i , 1:7] <- HD.data.current[[i]]$conditions
  #  SNCDladLasso.errors <- c(SNCDladLasso.errors , i)
  #  SNCDladLasso.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
  #  SNCDladLasso.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  #}
}


SNCDladLasso.maindf[ , "method"] <- "ladlasso"

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







#messing around and testing
SNCDladLasso.maindf <- data.frame(matrix(ncol = ncol(SNCDladLasso.final[[1]])))
colnames(SNCDladLasso.maindf) <- colnames(SNCDladLasso.final[[1]])
colnames(SNCDladLasso.maindf)
SNCDladLasso.maindf[1 , 1] <- as.numeric(as.character(SNCDladLasso.final[[1]][1 , 1]))

vec <- as.numeric(as.character(SNCDladLasso.final[[1]][1 , 1]))

SNCDladLasso.maindf[1 , ] <- apply(SNCDladLasso.final[[1]][1 , ] , 2 , function(x) as.numeric(as.character(x)))

as.numeric(as.character(SNCDladLasso.final[[1]][]))

class(SNCDladLasso.final[[1]])

