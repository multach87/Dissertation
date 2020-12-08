#load data
HD.data_DEBUG <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")
adalasso.HD.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/adalasso_HD_DEBUG.RData")


#split and store
#dealing with error/result from map(safely())
#create empty lists for error + result
adalasso.error <- list()
adalasso.result <- list()
adalasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(adalasso.HD.debug)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  adalasso.error[[i]] <- list(error = adalasso.HD.debug[[i]]$error , 
                              condition = as.data.frame(unlist(adalasso.HD.debug[[i]]$condition) , 
                                                        n = n , p = p , 
                                                        eta.x = eta.x , eta.y = eta.y , 
                                                        g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  adalasso.result[[i]] <- adalasso.HD.debug[[i]]$result
  #fill final list
  if(!is.null(adalasso.HD.debug[[i]]$result)) {
    adalasso.final[[i]] <- adalasso.HD.debug[[i]]$result$important
  } else {
    adalasso.final[[i]] <- adalasso.error[[i]]
  }
}

#save files
#saveRDS(adalasso.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/adalasso_result_HD_500.RData")
#saveRDS(adalasso.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adalasso_error_HD_500.RData")
#saveRDS(adalasso.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adalasso_resultmain_HD_500.RData")



#combine
#initialize dataframe
adalasso.results <- data.frame(matrix(ncol = ncol(adalasso.final[[1]]$info)))
colnames(adalasso.results) <- colnames(adalasso.final[[1]]$info)

#initialize error vector
adalasso.errors <- numeric()

#fill results
##Errors at: NONE
for(i in 1:length(adalasso.final)) {
  if(is.null(adalasso.final[[i]]$error)) {
    adalasso.results[i , ] <- adalasso.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    adalasso.results[i , 1:7] <- HD.data_DEBUG[[i]]$conditions
    adalasso.errors <- c(adalasso.errors , i)
  }
}

mean(adalasso.results[ , "fpr"] , na.rm = T)
mean(adalasso.results[ , "fnr"] , na.rm = T)
mean(adalasso.results[!is.infinite(adalasso.results[ , "mpe"]) , "mpe"] , na.rm = T)
length(adalasso.errors)

#save results
#saveRDS(adalasso.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adalasso_resultDF_HD_500.RData")
#saveRDS(adalasso.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adalasso_errorindices_HD_500.RData")

