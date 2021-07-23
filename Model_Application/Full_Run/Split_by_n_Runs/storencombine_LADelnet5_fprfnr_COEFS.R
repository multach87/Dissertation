#package libraries
library(dplyr)

#load data with fprfnr
SNCDladelnet5.all <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/mse_bias/msebias_ladelnet5_500_fprfnr_COEFS.RData")

SNCDladelnet5.error <- list()
SNCDladelnet5.result <- list()
SNCDladelnet5.final <- list()
#split data into separate error and result lists
for(i in 1:length(SNCDladelnet5.all)) { 
  #iteration tracker
  #cat("i = " , i , "\n")
  #fill error list
  SNCDladelnet5.error[[i]] <- list(error = SNCDladelnet5.all[[i]]$error , 
                                  condition = as.data.frame(SNCDladelnet5.all[[i]]$result))
  #fill in results if results aren't NULL from safely()
  SNCDladelnet5.result[[i]] <- SNCDladelnet5.all[[i]]$result
  #fill final list
  if(!is.null(SNCDladelnet5.all[[i]]$result)) {
    SNCDladelnet5.final[[i]] <- SNCDladelnet5.all[[i]]$result
  } else {
    cat("error at i = " , i , "/n")
    SNCDladelnet5.final[[i]] <- SNCDladelnet5.error[[i]]
  }
}

#initialize dataframe
SNCDladelnet5.maindf <- data.frame(matrix(ncol = ncol(SNCDladelnet5.final[[1]])))
colnames(SNCDladelnet5.maindf) <- colnames(SNCDladelnet5.final[[1]])
colnames(SNCDladelnet5.maindf)

#initialize error vector
SNCDladelnet5.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(SNCDladelnet5.final)) {   #length(S5NCDladelnet5.final)
  if((i %% 1000) == 0) {
    cat("i = " , i , "\n")
  }
  SNCDladelnet5.maindf[i , ] <- apply(SNCDladelnet5.final[[i]][1 , ] , 2 , 
                                     function(x) as.numeric(as.character(x)))
  #SNCDladelnet5.maindf[i , ] <- as.numeric(as.character(SNCDladelnet5.final[[1]][1 , ]))
  #if(is.null(SNCDladelnet5.final[[i]]$error)) {
  #  SNCDladelnet5.maindf[i , ] <- SNCDladelnet5.final[[i]]$result
  #} else {
  #  cat("error at i = " , i , "\n")
  #  SNCDladelnet5.maindf[i , 1:7] <- HD.data.current[[i]]$conditions
  #  SNCDladelnet5.errors <- c(SNCDladelnet5.errors , i)
  #  SNCDladelnet5.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
  #  SNCDladelnet5.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  #}
}


SNCDladelnet5.maindf[ , "method"] <- "ladelnet5"

#Check results
mean(SNCDladelnet5.maindf[ , "fpr"])
mean(SNCDladelnet5.maindf[ , "fpr"] , na.rm = T) #with null models included
mean(SNCDladelnet5.maindf[ , "fnr"])
mean(SNCDladelnet5.maindf[ , "fnr"] , na.rm = T) #with null models included


#save files
saveRDS(SNCDladelnet5.result , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/sncdladelnet5_result_500_COEFS.RData")
saveRDS(SNCDladelnet5.error , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/sncdladelnet5_error_500_COEFS.RData")
saveRDS(SNCDladelnet5.final , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/sncdladelnet5_resultmain_500_COEFS.RData")


saveRDS(SNCDladelnet5.maindf , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/sncdladelnet5_resultDF_500_COEFS.RData")
saveRDS(SNCDladelnet5.errors , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/sncdladelnet5_errorindices_500_COEFS.RData")







#messing around and testing
SNCDladelnet5.maindf <- data.frame(matrix(ncol = ncol(SNCDladelnet5.final[[1]])))
colnames(SNCDladelnet5.maindf) <- colnames(SNCDladelnet5.final[[1]])
colnames(SNCDladelnet5.maindf)
SNCDladelnet5.maindf[1 , 1] <- as.numeric(as.character(SNCDladelnet5.final[[1]][1 , 1]))

vec <- as.numeric(as.character(SNCDladelnet5.final[[1]][1 , 1]))

SNCDladelnet5.maindf[1 , ] <- apply(SNCDladelnet5.final[[1]][1 , ] , 2 , function(x) as.numeric(as.character(x)))

as.numeric(as.character(SNCDladelnet5.final[[1]][]))

class(SNCDladelnet5.final[[1]])

