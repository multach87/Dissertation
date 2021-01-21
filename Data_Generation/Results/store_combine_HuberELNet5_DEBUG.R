#load data
SNCDHuberELNet5.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/Huberelnet5_DEBUG.RData")

#load half data
DEBUG.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/debug_data_091620.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
SNCDHuberELNet5.error <- list()
SNCDHuberELNet5.result <- list()
SNCDHuberELNet5.final <- list()
#split data into separate error and result lists
for(i in 1:length(SNCDHuberELNet5.debug)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  SNCDHuberELNet5.error[[i]] <- list(error = SNCDHuberELNet5.debug[[i]]$error , 
                                 condition = as.data.frame(unlist(SNCDHuberELNet5.debug[[i]]$result$condition) , 
                                                           n = n , p = p , 
                                                           eta.x = eta.x , eta.y = eta.y , 
                                                           g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  SNCDHuberELNet5.result[[i]] <- SNCDHuberELNet5.debug[[i]]$result
  #fill final list
  if(!is.null(SNCDHuberELNet5.debug[[i]]$result)) {
    SNCDHuberELNet5.final[[i]] <- SNCDHuberELNet5.debug[[i]]$result$important
  } else {
    SNCDHuberELNet5.final[[i]] <- SNCDHuberELNet5.error[[i]]
  }
}



#initialize dataframe
SNCDHuberELNet5.maindf <- data.frame(matrix(ncol = ncol(SNCDHuberELNet5.final[[1]])))
colnames(SNCDHuberELNet5.maindf) <- colnames(SNCDHuberELNet5.final[[1]])

#initialize error vector
SNCDHuberELNet5.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(SNCDHuberELNet5.final)) {
  if(is.null(SNCDHuberELNet5.final[[i]]$error)) {
    SNCDHuberELNet5.maindf[i , ] <- SNCDHuberELNet5.final[[i]]
  } else {
    cat("error at i = " , i , "\n")
    SNCDHuberELNet5.maindf[i , 1:7] <- DEBUG.data[[i]]$conditions
    SNCDHuberELNet5.errors <- c(SNCDHuberELNet5.errors , i)
    SNCDHuberELNet5.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
    SNCDHuberELNet5.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  }
}

mean(SNCDHuberELNet5.maindf[ , "fpr"] , na.rm = T) #with null models included
mean(SNCDHuberELNet5.maindf[ , "fnr"] , na.rm = T) #with null models included
mean(SNCDHuberELNet5.maindf[!is.infinite(SNCDHuberELNet5.maindf[ , "mpe"]) , "mpe"] , na.rm = T) #can only calculate without null models due to error
mean(SNCDHuberELNet5.maindf[-SNCDHuberELNet5.errors , "fpr"] , na.rm = T) #null models excluded
mean(SNCDHuberELNet5.maindf[-SNCDHuberELNet5.errors , "fnr"] , na.rm = T) #null models excluded

#save results
#saveRDS(SNCDHuberELNet5.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/SNCDHuberLassoHD_result_DEBUG.RData")
saveRDS(SNCDHuberELNet5.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/SNCDHuberELNet5_error_DEBUG.RData")
saveRDS(SNCDHuberELNet5.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/SNCDHuberELNet5_resultmain_DEBUG.RData")
saveRDS(SNCDHuberELNet5.maindf , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/SNCDHuberELNet5_resultDF_DEBUG.RData")
#saveRDS(SNCDHuberELNet5.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/SNCDHuberLassoHD_errorindices_500.RData")
