#load data
HD.data_DEBUG <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")
msaelnet9.HD.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet9_HD_DEBUG.RData")




#split and store results
#dealing with error/result from map(safely())
#create empty lists for error + result
msaelnet9.error <- list()
msaelnet9.result <- list()
msaelnet9.final <- list()
#split data into separate error and result lists
for(i in 1:length(msaelnet9.HD.debug)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  msaelnet9.error[[i]] <- list(error = msaelnet9.HD.debug[[i]]$error , 
                               condition = as.data.frame(unlist(msaelnet9.HD.debug[[i]]$result$condition) , 
                                                         n = n , p = p , 
                                                         eta.x = eta.x , eta.y = eta.y , 
                                                         g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  msaelnet9.result[[i]] <- msaelnet9.HD.debug[[i]]$result
  #fill final list
  if(!is.null(msaelnet9.HD.debug[[i]]$result)) {
    msaelnet9.final[[i]] <- msaelnet9.HD.debug[[i]]$result$important
  } else {
    msaelnet9.final[[i]] <- msaelnet9.error[[i]]
  }
}



#initialize dataframe
msaelnet9.maindf <- data.frame(matrix(ncol = ncol(msaelnet9.final[[1]]$info)))
colnames(msaelnet9.maindf) <- colnames(msaelnet9.final[[1]]$info)

#initialize error vector
msaelnet9.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(msaelnet9.final)) {
  if(is.null(msaelnet9.final[[i]]$error)) {
    msaelnet9.maindf[i , ] <- msaelnet9.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    msaelnet9.maindf[i , 1:7] <- half.data[[i]]$conditions
    msaelnet9.errors <- c(msaelnet9.errors , i)
    msaelnet9.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
    msaelnet9.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  }
}

mean(msaelnet9.maindf[ , "fpr"] , na.rm = T) #with null models included
mean(msaelnet9.maindf[ , "fnr"] , na.rm = T) #with null models included
mean(msaelnet9.maindf[!is.infinite(msaelnet9.maindf[ , "mpe"]) , "mpe"] , na.rm = T) #can only calculate without null models due to error
mean(msaelnet9.maindf[-msaelnet9.errors , "fpr"] , na.rm = T) #null models excluded
mean(msaelnet9.maindf[-msaelnet9.errors , "fnr"] , na.rm = T) #null models excluded
length(msaelnet9.errors)

#save results
#saveRDS(msaelnet9.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/msaelnet9_result_HD_500.RData")
#saveRDS(msaelnet9.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/msaelnet9_error_HD_500.RData")
#saveRDS(msaelnet9.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msaelnet9_resultmain_HD_500.RData")
#saveRDS(msaelnet9.maindf , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msaelnet9_resultDF_HD_500.RData")
#saveRDS(msaelnet9.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/msaelnet9_errorindices_HD_500.RData")
