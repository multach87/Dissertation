#load data
outlier25.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/outlier25.RData")
outlier50.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/outlier50.RData")
outlier100.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/outlier100.RData")



pense5_outlier25 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/pense5_outlier25.RData")
#split and store results

#dealing with error/result from map(safely())
#create empty lists for error + result
elnet5.error <- list()
elnet5.result <- list()
elnet5.final <- list()
#split data into separate error and result lists
for(i in 1:length(elnet5.HD.HALF)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  elnet5.error[[i]] <- list(error = elnet5.HD.HALF[[i]]$error , 
                            condition = as.data.frame(unlist(elnet5.HD.HALF[[i]]$result$condition) , 
                                                      n = n , p = p , 
                                                      eta.x = eta.x , eta.y = eta.y , 
                                                      g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  elnet5.result[[i]] <- elnet5.HD.HALF[[i]]$result
  #fill final list
  if(!is.null(elnet5.HD.HALF[[i]]$result)) {
    elnet5.final[[i]] <- elnet5.HD.HALF[[i]]$result$important
  } else {
    elnet5.final[[i]] <- elnet5.error[[i]]
  }
}

#save files
#saveRDS(elnet5.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/elnet5_result_HD_500.RData")
#saveRDS(elnet5.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet5_error_HD_500.RData")
#saveRDS(elnet5.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet5_resultmain_HD_500.RData")







#combine main results
#initialize dataframe
elnet5.results <- data.frame(matrix(ncol = ncol(elnet5.final[[1]]$info)))
colnames(elnet5.results) <- colnames(elnet5.final[[1]]$info)

#initialize error vector
elnet5.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(elnet5.final)) {
  if(is.null(elnet5.final[[i]]$error)) {
    elnet5.results[i , ] <- elnet5.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    elnet5.results[i , 1:7] <- HD.halfdata[[i]]$conditions
    elnet5.errors <- c(elnet5.errors , i)
  }
}

mean(elnet5.results[ , "fpr"] , na.rm = T)
mean(elnet5.results[ , "fnr"] , na.rm = T)
mean(elnet5.results[!is.infinite(elnet5.results[ , "mpe"]) , "mpe"] , na.rm = T)
length(elnet5.errors)

#save results
#saveRDS(elnet5.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet5_resultDF_HD_500.RData")
#saveRDS(elnet5.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet5_errorindices_HD_500.RData")