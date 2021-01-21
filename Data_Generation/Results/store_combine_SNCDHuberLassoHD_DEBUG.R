#load data
SNCDHuberLasso.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/")

#load half data
DEBUG.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/debug_data_091620.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
SNCDHuberLasso.error <- list()
SNCDHuberLasso.result <- list()
SNCDHuberLasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(SNCDHuberLasso.debug)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  SNCDHuberLasso.error[[i]] <- list(error = SNCDHuberLasso.debug[[i]]$error , 
                                 condition = as.data.frame(unlist(SNCDHuberLasso.debug[[i]]$result$condition) , 
                                                           n = n , p = p , 
                                                           eta.x = eta.x , eta.y = eta.y , 
                                                           g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  SNCDHuberLasso.result[[i]] <- SNCDHuberLasso.debug[[i]]$result
  #fill final list
  if(!is.null(SNCDHuberLasso.debug[[i]]$result)) {
    SNCDHuberLasso.final[[i]] <- SNCDHuberLasso.debug[[i]]$result$important
  } else {
    SNCDHuberLasso.final[[i]] <- SNCDHuberLasso.error[[i]]
  }
}



#initialize dataframe
SNCDHuberLasso.maindf <- data.frame(matrix(ncol = ncol(SNCDHuberLasso.final[[1]])))
colnames(SNCDHuberLasso.maindf) <- colnames(SNCDHuberLasso.final[[1]])

#initialize error vector
SNCDHuberLasso.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(SNCDHuberLasso.final)) {
  if(is.null(SNCDHuberLasso.final[[i]]$error)) {
    SNCDHuberLasso.maindf[i , ] <- SNCDHuberLasso.final[[i]]
  } else {
    cat("error at i = " , i , "\n")
    SNCDHuberLasso.maindf[i , 1:7] <- HD.data_debug[[i]]$conditions
    SNCDHuberLasso.errors <- c(SNCDHuberLasso.errors , i)
    SNCDHuberLasso.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
    SNCDHuberLasso.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  }
}

mean(SNCDHuberLasso.maindf[ , "fpr"] , na.rm = T) #with null models included
mean(SNCDHuberLasso.maindf[ , "fnr"] , na.rm = T) #with null models included
mean(SNCDHuberLasso.maindf[!is.infinite(SNCDHuberLasso.maindf[ , "mpe"]) , "mpe"] , na.rm = T) #can only calculate without null models due to error
mean(SNCDHuberLasso.maindf[-SNCDHuberLasso.errors , "fpr"] , na.rm = T) #null models excluded
mean(SNCDHuberLasso.maindf[-SNCDHuberLasso.errors , "fnr"] , na.rm = T) #null models excluded

#save results
saveRDS(SNCDHuberLasso.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/SNCDHuberLasso_result_DEBUG.RData")
saveRDS(SNCDHuberLasso.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/SNCDHuberLasso_error_DEBUG.RData")
saveRDS(SNCDHuberLasso.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/SNCDHuberLasso_resultmain_DEBUG.RData")
saveRDS(SNCDHuberLasso.maindf , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/SNCDHuberLasso_resultDF_500.RData")
#saveRDS(SNCDHuberLasso.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/SNCDHuberLasso_errorindices_500.RData")
