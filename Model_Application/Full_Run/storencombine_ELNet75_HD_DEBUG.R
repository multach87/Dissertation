#load data
HD.data_DEBUG <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")
elnet75.HD.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/elnet75_HD_DEBUG.RData")



#split and store results

#dealing with error/result from map(safely())
#create empty lists for error + result
elnet75.error <- list()
elnet75.result <- list()
elnet75.final <- list()
#split data into separate error and result lists
for(i in 1:length(elnet75.HD.debug)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  elnet75.error[[i]] <- list(error = elnet75.HD.debug[[i]]$error , 
                             condition = as.data.frame(unlist(elnet75.HD.debug[[i]]$result$condition) , 
                                                       n = n , p = p , 
                                                       eta.x = eta.x , eta.y = eta.y , 
                                                       g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  elnet75.result[[i]] <- elnet75.HD.debug[[i]]$result
  #fill final list
  if(!is.null(elnet75.HD.debug[[i]]$result)) {
    elnet75.final[[i]] <- elnet75.HD.debug[[i]]$result$important
  } else {
    elnet75.final[[i]] <- elnet75.error[[i]]
  }
}

#save files
#saveRDS(elnet75.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/elnet75_result_HD_500.RData")
#saveRDS(elnet75.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet75_error_HD_500.RData")
#saveRDS(elnet75.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet75_resultmain_HD_500.RData")







#combine main results
#initialize dataframe
elnet75.results <- data.frame(matrix(ncol = ncol(elnet75.final[[1]]$info)))
colnames(elnet75.results) <- colnames(elnet75.final[[1]]$info)

#initialize error vector
elnet75.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(elnet75.final)) {
  if(is.null(elnet75.final[[i]]$error)) {
    elnet75.results[i , ] <- elnet75.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    elnet75.results[i , 1:7] <- HD.data_DEBUG[[i]]$conditions
    elnet75.errors <- c(elnet75.errors , i)
  }
}

mean(elnet75.results[ , "fpr"] , na.rm = T)
mean(elnet75.results[ , "fnr"] , na.rm = T)
mean(elnet75.results[!is.infinite(elnet75.results[ , "mpe"]) , "mpe"] , na.rm = T)
length(elnet75.errors)

#save results
#saveRDS(elnet75.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet75_resultDF_HD_500.RData")
#saveRDS(elnet75.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet75_errorindices_HD_500.RData")



