#load data
HD.data_DEBUG <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")
msaelnet75.HD.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/msaelnet75_HD_DEBUG.RData")



#split and store results
#dealing with error/result from map(safely())
#create empty lists for error + result
msaelnet75.error <- list()
msaelnet75.result <- list()
msaelnet75.final <- list()
#split data into separate error and result lists
for(i in 1:length(msaelnet75.HD.debug)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  msaelnet75.error[[i]] <- list(error = msaelnet75.HD.debug[[i]]$error , 
                                condition = as.data.frame(unlist(msaelnet75.HD.debug[[i]]$result$condition) , 
                                                          n = n , p = p , 
                                                          eta.x = eta.x , eta.y = eta.y , 
                                                          g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  msaelnet75.result[[i]] <- msaelnet75.HD.debug[[i]]$result
  #fill final list
  if(!is.null(msaelnet75.HD.debug[[i]]$result)) {
    msaelnet75.final[[i]] <- msaelnet75.HD.debug[[i]]$result$important
  } else {
    msaelnet75.final[[i]] <- msaelnet75.error[[i]]
  }
}



#initialize dataframe
msaelnet75.maindf <- data.frame(matrix(ncol = ncol(msaelnet75.final[[1]]$info)))
colnames(msaelnet75.maindf) <- colnames(msaelnet75.final[[1]]$info)

#initialize error vector
msaelnet75.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(msaelnet75.final)) {
  if(is.null(msaelnet75.final[[i]]$error)) {
    msaelnet75.maindf[i , ] <- msaelnet75.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    msaelnet75.maindf[i , 1:7] <- HD.data_DEBUG[[i]]$conditions
    msaelnet75.errors <- c(msaelnet75.errors , i)
    msaelnet75.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
    msaelnet75.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  }
}

mean(msaelnet75.maindf[ , "fpr"] , na.rm = T) #with null models included
mean(msaelnet75.maindf[ , "fnr"] , na.rm = T) #with null models included
mean(msaelnet75.maindf[!is.infinite(msaelnet75.maindf[ , "mpe"]) , "mpe"] , na.rm = T) #can only calculate without null models due to error
mean(msaelnet75.maindf[-msaelnet75.errors , "fpr"] , na.rm = T) #null models excluded
mean(msaelnet75.maindf[-msaelnet75.errors , "fnr"] , na.rm = T) #null models excluded
length(msaelnet75.errors)

#save results
#saveRDS(msaelnet75.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/msaelnet75_result_HD_500.RData")
#saveRDS(msaelnet75.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/msaelnet75_error_HD_500.RData")
#saveRDS(msaelnet75.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msaelnet75_resultmain_HD_500.RData")
#saveRDS(msaelnet75.maindf , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msaelnet75_resultDF_HD_500.RData")
#saveRDS(msaelnet75.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/msaelnet75_errorindices_HD_500.RData")


