#load data
HD.halfdata <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HDSparsedata_112320.RData")

OSLassoPLUS.HD.HALF <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/OSlassoPLUS_HD_500.RData")


#split and store results
#dealing with error/result from map(safely())
#create empty lists for error + result
OSLassoPLUS.error <- list()
OSLassoPLUS.result <- list()
OSLassoPLUS.final <- list()
#split data into separate error and result lists
for(i in 1:length(OSLassoPLUS.HD.HALF)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  OSLassoPLUS.error[[i]] <- list(error = OSLassoPLUS.HD.HALF[[i]]$error , 
                                 condition = as.data.frame(unlist(OSLassoPLUS.HD.HALF[[i]]$result$condition) , 
                                                           n = n , p = p , 
                                                           eta.x = eta.x , eta.y = eta.y , 
                                                           g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  OSLassoPLUS.result[[i]] <- OSLassoPLUS.HD.HALF[[i]]$result
  #fill final list
  if(!is.null(OSLassoPLUS.HD.HALF[[i]]$result)) {
    OSLassoPLUS.final[[i]] <- OSLassoPLUS.HD.HALF[[i]]$result$important
  } else {
    OSLassoPLUS.final[[i]] <- OSLassoPLUS.error[[i]]
  }
}


#save files
#saveRDS(OSLassoPLUS.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/OS_result_HD_500.RData")
#saveRDS(OSLassoPLUS.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/OS_error_HD_500.RData")
#saveRDS(OSLassoPLUS.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/OS_resultmain_HD_500.RData")




#combine results
#initialize dataframe
OS.results <- data.frame(matrix(ncol = ncol(OSLassoPLUS.final[[1]]$info)))
colnames(OS.results) <- colnames(OSLassoPLUS.final[[1]]$info)

#initialize error vector
OSLassoPLUS.errors <- numeric()

#fill results
##ERRORS AT: 
for(i in 1:length(OSLassoPLUS.final)) {
  if(is.null(OSLassoPLUS.final[[i]]$error)) {
    OS.results[i , ] <- OSLassoPLUS.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    OS.results[i , 1:7] <- HD.halfdata[[i]]$conditions
    OSLassoPLUS.errors <- c(OSLassoPLUS.errors , i)
  }
}

mean(OS.results[ , "fpr"] , na.rm = T)
mean(OS.results[ , "fnr"] , na.rm = T)
mean(OS.results[!is.infinite(OS.results[ , "mpe"]) , "mpe"] , na.rm = T)
length(OSLassoPLUS.errors)


saveRDS(OS.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/OSLassoPLUS_resultDF_HD_500.RData")
saveRDS(OSLassoPLUS.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/OSLassoPLUS_errorindices_HD_500.RData")

