#load data
adalassoHD.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/adalasso_HD_DEBUG.RData")

#load half data
HD.data_debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
adalassoHD.error <- list()
adalassoHD.result <- list()
adalassoHD.final <- list()
#split data into separate error and result lists
for(i in 1:length(adalassoHD.debug)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  adalassoHD.error[[i]] <- list(error = adalassoHD.debug[[i]]$error , 
                                 condition = as.data.frame(unlist(adalassoHD.debug[[i]]$result$condition) , 
                                                           n = n , p = p , 
                                                           eta.x = eta.x , eta.y = eta.y , 
                                                           g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  adalassoHD.result[[i]] <- adalassoHD.debug[[i]]$result
  #fill final list
  if(!is.null(adalassoHD.debug[[i]]$result)) {
    adalassoHD.final[[i]] <- adalassoHD.debug[[i]]$result$important
  } else {
    adalassoHD.final[[i]] <- adalassoHD.error[[i]]
  }
}



#initialize dataframe
adalassoHD.maindf <- data.frame(matrix(ncol = ncol(adalassoHD.final[[1]]$info)))
colnames(adalassoHD.maindf) <- colnames(adalassoHD.final[[1]]$info)

#initialize error vector
adalassoHD.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(adalassoHD.final)) {
  if(is.null(adalassoHD.final[[i]]$error)) {
    adalassoHD.maindf[i , ] <- adalassoHD.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    adalassoHD.maindf[i , 1:7] <- half.data[[i]]$conditions
    adalassoHD.errors <- c(adalassoHD.errors , i)
    adalassoHD.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
    adalassoHD.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  }
}

mean(adalassoHD.maindf[ , "fpr"] , na.rm = T) #with null models included
mean(adalassoHD.maindf[ , "fnr"] , na.rm = T) #with null models included
mean(adalassoHD.maindf[!is.infinite(adalassoHD.maindf[ , "mpe"]) , "mpe"] , na.rm = T) #can only calculate without null models due to error
mean(adalassoHD.maindf[-adalassoHD.errors , "fpr"] , na.rm = T) #null models excluded
mean(adalassoHD.maindf[-adalassoHD.errors , "fnr"] , na.rm = T) #null models excluded

#save results
saveRDS(adalassoHD.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/adalassoHD_result_DEBUG.RData")
saveRDS(adalassoHD.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adalassoHD_error_DEBUG.RData")
saveRDS(adalassoHD.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adalassoHD_resultmain_DEBUG.RData")
saveRDS(adalassoHD.maindf , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adalassoHD_resultDF_500.RData")
#saveRDS(adalassoHD.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adalassoHD_errorindices_500.RData")
