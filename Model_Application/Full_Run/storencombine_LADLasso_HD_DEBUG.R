#load data
HD.data_DEBUG <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")
ladlasso.HD.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_DEBUG.RData")


#split and store results

#dealing with error/result from map(safely())
#create empty lists for error + result
ladlasso.error <- list()
ladlasso.result <- list()
ladlasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(ladlasso.HD.debug)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  ladlasso.error[[i]] <- list(error = ladlasso.HD.debug[[i]]$error , 
                              condition = as.data.frame(unlist(ladlasso.HD.debug[[i]]$condition) , 
                                                        n = n , p = p , 
                                                        eta.x = eta.x , eta.y = eta.y , 
                                                        g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  ladlasso.result[[i]] <- ladlasso.HD.debug[[i]]$result
  #fill final list
  if(!is.null(ladlasso.HD.debug[[i]]$result)) {
    ladlasso.final[[i]] <- ladlasso.HD.debug[[i]]$result$important
  } else {
    ladlasso.final[[i]] <- ladlasso.error[[i]]
  }
}

#save files
#saveRDS(ladlasso.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/ladlasso_result_HD_500.RData")
#saveRDS(ladlasso.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/ladlasso_error_HD_500.RData")
#saveRDS(ladlasso.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/ladlasso_resultmain_HD_500.RData")



#combine

#initialize dataframe
ladlasso.results <- data.frame(matrix(ncol = ncol(ladlasso.final[[1]]$info)))
colnames(ladlasso.results) <- colnames(ladlasso.final[[1]]$info)

#initialize error vector
ladlasso.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(ladlasso.final)) {
  if(is.null(ladlasso.final[[i]]$error)) {
    ladlasso.results[i , ] <- ladlasso.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    ladlasso.results[i , 1:7] <- HD.data_DEBUG[[i]]$conditions
    ladlasso.errors <- c(ladlasso.errors , i)
  }
}

mean(ladlasso.results[ , "fpr"] , na.rm = T)
mean(ladlasso.results[ , "fnr"] , na.rm = T)
mean(ladlasso.results[!is.infinite(ladlasso.results[ , "mpe"]) , "mpe"] , na.rm = T)
length(ladlasso.errors)

#save results
#saveRDS(ladlasso.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/ladlasso_resultDF_HD_500.RData")
#saveRDS(ladlasso.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/ladlasso_errorindices_HD_500.RData")
