#load data
HD.data_DEBUG <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")
OSLassoPLUS.HD.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/OSlassoPLUS_HD_DEBUG.RData")


#split and store results
#dealing with error/result from map(safely())
#create empty lists for error + result
OSLassoPLUS.error <- list()
OSLassoPLUS.result <- list()
OSLassoPLUS.final <- list()
#split data into separate error and result lists
for(i in 1:length(OSLassoPLUS.HD.debug)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  OSLassoPLUS.error[[i]] <- list(error = OSLassoPLUS.HD.debug[[i]]$error , 
                                 condition = as.data.frame(unlist(OSLassoPLUS.HD.debug[[i]]$result$condition) , 
                                                           n = n , p = p , 
                                                           eta.x = eta.x , eta.y = eta.y , 
                                                           g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  OSLassoPLUS.result[[i]] <- OSLassoPLUS.HD.debug[[i]]$result
  #fill final list
  if(!is.null(OSLassoPLUS.HD.debug[[i]]$result)) {
    OSLassoPLUS.final[[i]] <- OSLassoPLUS.HD.debug[[i]]$result$important
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
OS.results <- data.frame(matrix(ncol = ncol(OS.final[[1]]$info)))
colnames(OS.results) <- colnames(OS.final[[1]]$info)

#initialize error vector
OSLassoPLUS.errors <- numeric()

#fill results
##ERRORS AT: 
for(i in 1:length(OS.final)) {
  if(is.null(OS.final[[i]]$error)) {
    OS.results[i , ] <- OS.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    OS.results[i , 1:7] <- HD.data_DEBUG[[i]]$conditions
    OSLassoPLUS.errors <- c(OSLassoPLUS.errors , i)
  }
}

mean(OS.results[ , "fpr"] , na.rm = T)
mean(OS.results[ , "fnr"] , na.rm = T)
mean(OS.results[!is.infinite(OS.results[ , "mpe"]) , "mpe"] , na.rm = T)
length(OSLassoPLUS.errors)


#saveRDS(OS.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/OSLassoPLUS_resultDF_HD_500.RData")
#saveRDS(OSLassoPLUS.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/OSLassoPLUS_errorindices_HD_500.RData")

