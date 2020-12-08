#load data
HD.data_DEBUG <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")
elnet9.HD.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/elnet9_HD_DEBUG.RData")



#split and store results

#dealing with error/result from map(safely())
#create empty lists for error + result
elnet9.error <- list()
elnet9.result <- list()
elnet9.final <- list()
#split data into separate error and result lists
for(i in 1:length(elnet9.HD.debug)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  elnet9.error[[i]] <- list(error = elnet9.HD.debug[[i]]$error , 
                            condition = as.data.frame(unlist(elnet9.HD.debug[[i]]$result$condition) , 
                                                      n = n , p = p , 
                                                      eta.x = eta.x , eta.y = eta.y , 
                                                      g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  elnet9.result[[i]] <- elnet9.HD.debug[[i]]$result
  #fill final list
  if(!is.null(elnet9.HD.debug[[i]]$result)) {
    elnet9.final[[i]] <- elnet9.HD.debug[[i]]$result$important
  } else {
    elnet9.final[[i]] <- elnet9.error[[i]]
  }
}

#save files
#saveRDS(elnet9.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/elnet9_result_HD_500.RData")
#saveRDS(elnet9.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet9_error_HD_500.RData")
#saveRDS(elnet9.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet9_resultmain_HD_500.RData")







#combine main results
#initialize dataframe
elnet9.results <- data.frame(matrix(ncol = ncol(elnet9.final[[1]]$info)))
colnames(elnet9.results) <- colnames(elnet9.final[[1]]$info)

#initialize error vector
elnet9.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(elnet9.final)) {
  if(is.null(elnet9.final[[i]]$error)) {
    elnet9.results[i , ] <- elnet9.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    elnet9.results[i , 1:7] <- HD.data_DEBUG[[i]]$conditions
    elnet9.errors <- c(elnet9.errors , i)
  }
}

mean(elnet9.results[ , "fpr"] , na.rm = T)
mean(elnet9.results[ , "fnr"] , na.rm = T)
mean(elnet9.results[!is.infinite(elnet9.results[ , "mpe"]) , "mpe"] , na.rm = T)
length(elnet9.errors)

#save results
#saveRDS(elnet9.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet9_resultDF_HD_500.RData")
#saveRDS(elnet9.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet9_errorindices_HD_500.RData")
