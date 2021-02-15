#load data
HD.halfdata <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HDSparsedata_112320.RData")

msaelnet5.HD.HALF <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_HD_500.RData")




#split and store results
#dealing with error/result from map(safely())
#create empty lists for error + result
msaelnet5.error <- list()
msaelnet5.result <- list()
msaelnet5.final <- list()
#split data into separate error and result lists
for(i in 1:length(msaelnet5.HD.HALF)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  msaelnet5.error[[i]] <- list(error = msaelnet5.HD.HALF[[i]]$error , 
                               condition = as.data.frame(unlist(msaelnet5.HD.HALF[[i]]$result$condition) , 
                                                         n = n , p = p , 
                                                         eta.x = eta.x , eta.y = eta.y , 
                                                         g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  msaelnet5.result[[i]] <- msaelnet5.HD.HALF[[i]]$result
  #fill final list
  if(!is.null(msaelnet5.HD.HALF[[i]]$result)) {
    msaelnet5.final[[i]] <- msaelnet5.HD.HALF[[i]]$result$important
  } else {
    msaelnet5.final[[i]] <- msaelnet5.error[[i]]
  }
}



#initialize dataframe
msaelnet5.maindf <- data.frame(matrix(ncol = ncol(msaelnet5.final[[1]]$info)))
colnames(msaelnet5.maindf) <- colnames(msaelnet5.final[[1]]$info)

#initialize error vector
msaelnet5.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(msaelnet5.final)) {
  if(is.null(msaelnet5.final[[i]]$error)) {
    msaelnet5.maindf[i , ] <- msaelnet5.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    msaelnet5.maindf[i , 1:7] <- HD.halfdata[[i]]$conditions
    msaelnet5.errors <- c(msaelnet5.errors , i)
    msaelnet5.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
    msaelnet5.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  }
}

mean(msaelnet5.maindf[ , "fpr"] , na.rm = T) #with null models included
mean(msaelnet5.maindf[ , "fnr"] , na.rm = T) #with null models included
mean(msaelnet5.maindf[!is.infinite(msaelnet5.maindf[ , "mpe"]) , "mpe"] , na.rm = T) #can only calculate without null models due to error
mean(msaelnet5.maindf[-msaelnet5.errors , "fpr"] , na.rm = T) #null models excluded
mean(msaelnet5.maindf[-msaelnet5.errors , "fnr"] , na.rm = T) #null models excluded
length(msaelnet5.errors)

#save results
#saveRDS(msaelnet5.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/msaelnet5_result_HD_500.RData")
#saveRDS(msaelnet5.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/msaelnet5_error_HD_500.RData")
#saveRDS(msaelnet5.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msaelnet5_resultmain_HD_500.RData")
saveRDS(msaelnet5.maindf , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msaelnet5_resultDF_HD_500.RData")
saveRDS(msaelnet5.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/msaelnet5_errorindices_HD_500.RData")


