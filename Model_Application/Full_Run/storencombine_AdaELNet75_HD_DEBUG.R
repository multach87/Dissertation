#load data
HD.data_DEBUG <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")
adaelnet75.HD.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/adaelnet75_HD_DEBUG.RData")









#split and store results
#dealing with error/result from map(safely())
#create empty lists for error + result
adaelnet75.error <- list()
adaelnet75.result <- list()
adaelnet75.final <- list()
#split data into separate error and result lists
for(i in 1:length(adaelnet75.HD.debug)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  adaelnet75.error[[i]] <- list(error = adaelnet75.HD.debug[[i]]$error , 
                                condition = as.data.frame(unlist(adaelnet75.HD.debug[[i]]$result$condition) , 
                                                          n = n , p = p , 
                                                          eta.x = eta.x , eta.y = eta.y , 
                                                          g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  adaelnet75.result[[i]] <- adaelnet75.HD.debug[[i]]$result
  #fill final list
  if(!is.null(adaelnet75.HD.debug[[i]]$result)) {
    adaelnet75.final[[i]] <- adaelnet75.HD.debug[[i]]$result$important
  } else {
    adaelnet75.final[[i]] <- adaelnet75.error[[i]]
  }
}


#save files
#saveRDS(adaelnet75.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/adaelnet75_result_HD_500.RData")
#saveRDS(adaelnet75.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adaelnet75_error_HD_500.RData")
#saveRDS(adaelnet75.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet75_resultmain_HD_500.RData")



#combine results
#initialize dataframe
adaelnet75.results <- data.frame(matrix(ncol = ncol(adaelnet75.final[[1]]$info)))
colnames(adaelnet75.results) <- colnames(adaelnet75.final[[1]]$info)

#initialize error vector
adaelnet75.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(adaelnet75.final)) {
  if(is.null(adaelnet75.final[[i]]$error)) {
    adaelnet75.results[i , ] <- adaelnet75.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    adaelnet75.results[i , 1:7] <- HD.data_DEBUG[[i]]$conditions
    adaelnet75.errors <- c(adaelnet75.errors , i)
  }
}

mean(adaelnet75.results[ , "fpr"] , na.rm = T)
mean(adaelnet75.results[ , "fnr"] , na.rm = T)
mean(adaelnet75.results[!is.infinite(adaelnet75.results[ , "mpe"]) , "mpe"] , na.rm = T)
length(adaelnet75.errors)

#save results
#saveRDS(adaelnet75.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet75_resultDF_HD_500.RData")
#saveRDS(adaelnet75.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adaelnet75_errorindices_HD_500.RData")
