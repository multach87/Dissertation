#load data
HD.data_DEBUG <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")
adaelnet9.HD.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/adaelnet9_HD_DEBUG.RData")









#split and store results
#dealing with error/result from map(safely())
#create empty lists for error + result
adaelnet9.error <- list()
adaelnet9.result <- list()
adaelnet9.final <- list()
#split data into separate error and result lists
for(i in 1:length(adaelnet9.HD.debug)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  adaelnet9.error[[i]] <- list(error = adaelnet9.HD.debug[[i]]$error , 
                               condition = as.data.frame(unlist(adaelnet9.HD.debug[[i]]$result$condition) , 
                                                         n = n , p = p , 
                                                         eta.x = eta.x , eta.y = eta.y , 
                                                         g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  adaelnet9.result[[i]] <- adaelnet9.HD.debug[[i]]$result
  #fill final list
  if(!is.null(adaelnet9.HD.debug[[i]]$result)) {
    adaelnet9.final[[i]] <- adaelnet9.HD.debug[[i]]$result$important
  } else {
    adaelnet9.final[[i]] <- adaelnet9.error[[i]]
  }
}


#save files
#saveRDS(adaelnet9.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/adaelnet9_result_HD_500.RData")
#saveRDS(adaelnet9.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adaelnet9_error_HD_500.RData")
#saveRDS(adaelnet9.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet9_resultmain_HD_500.RData")



#combine results
#initialize dataframe
adaelnet9.results <- data.frame(matrix(ncol = ncol(adaelnet9.final[[1]]$info)))
colnames(adaelnet9.results) <- colnames(adaelnet9.final[[1]]$info)

#initialize error vector
adaelnet9.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(adaelnet9.final)) {
  if(is.null(adaelnet9.final[[i]]$error)) {
    adaelnet9.results[i , ] <- adaelnet9.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    adaelnet9.results[i , 1:7] <- HD.data_DEBUG[[i]]$conditions
    adaelnet9.errors <- c(adaelnet9.errors , i)
  }
}

mean(adaelnet9.results[ , "fpr"] , na.rm = T)
mean(adaelnet9.results[ , "fnr"] , na.rm = T)
mean(adaelnet9.results[!is.infinite(adaelnet9.results[ , "mpe"]) , "mpe"] , na.rm = T)
length(adaelnet9.errors)

#save results
#saveRDS(adaelnet9.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet9_resultDF_HD_500.RData")
#saveRDS(adaelnet9.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adaelnet9_errorindices_HD_500.RData")
