#load data
HD.data_DEBUG <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")
lasso.HD.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/lasso_HD_DEBUG.RData")


#storing

#dealing with error/result from map(safely())
#create empty lists for error + result
lasso.error <- list()
lasso.result <- list()
lasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(lasso.HD.debug)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  lasso.error[[i]] <- list(error = lasso.HD.debug[[i]]$error , 
                           condition = as.data.frame(unlist(lasso.HD.debug[[i]]$condition) , 
                                                     n = n , p = p , 
                                                     eta.x = eta.x , eta.y = eta.y , 
                                                     g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  lasso.result[[i]] <- lasso.HD.debug[[i]]$result
  #fill final list
  if(!is.null(lasso.HD.debug[[i]]$result)) {
    lasso.final[[i]] <- lasso.HD.debug[[i]]$result$important
  } else {
    lasso.final[[i]] <- lasso.error[[i]]
  }
}


#save files
#saveRDS(lasso.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/lasso_result_HD_DEBUG.RData")
#saveRDS(lasso.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/lasso_error_HD_DEBUG.RData")
#saveRDS(lasso.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/lasso_resultmain_HD_DEBUG.RData")

#combine

#initialize dataframe
lasso.results <- data.frame(matrix(ncol = ncol(lasso.final[[1]]$info)))
colnames(lasso.results) <- colnames(lasso.final[[1]]$info)

#initialize error vector
lasso.errors <- numeric()

#fill results
for(i in 1:length(lasso.final)) {
  if(is.null(lasso.final[[i]]$error)) {
    lasso.results[i , ] <- lasso.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    lasso.results[i , 1:7] <- full.data[[i]]$conditions
    lasso.errors <- c(lasso.errors , i)
  }
}

mean(lasso.results[ , "fpr"] , na.rm = T)
mean(lasso.results[ , "fnr"] , na.rm = T)
mean(lasso.results[ , "mpe"] , na.rm = T)
length(lasso.errors)

#saveRDS(lasso.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/Lasso_resultDF_FULL.RData")
#saveRDS(lasso.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/lasso_errorindices_HD_500.RData")


