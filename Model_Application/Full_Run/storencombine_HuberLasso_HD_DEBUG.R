#load data
HD.data_DEBUG <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")
HuberLasso.HD.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/HuberLasso_HD_DEBUG.RData")


#split and store results
#dealing with error/result from map(safely())
#create empty lists for error + result
Huber.error <- list()
Huber.result <- list()
Huber.final <- list()
#split data into separate error and result lists
for(i in 1:length(HuberLasso.HD.debug)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  Huber.error[[i]] <- list(error = HuberLasso.HD.debug[[i]]$error , 
                           condition = as.data.frame(unlist(HuberLasso.HD.debug[[i]]$result$condition) , 
                                                     n = n , p = p , 
                                                     eta.x = eta.x , eta.y = eta.y , 
                                                     g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  Huber.result[[i]] <- HuberLasso.HD.debug[[i]]$result
  #fill final list
  if(!is.null(HuberLasso.HD.debug[[i]]$result)) {
    Huber.final[[i]] <- HuberLasso.HD.debug[[i]]$result$important
  } else {
    Huber.final[[i]] <- Huber.error[[i]]
  }
}

#save files
#saveRDS(Huber.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/Huber_result_HD_500.RData")
#saveRDS(Huber.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/Huber_error_HD_500.RData")
#saveRDS(Huber.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/Huber_resultmain_HD_500.RData")



#combine results
#initialize dataframe
Huber.results <- data.frame(matrix(ncol = ncol(Huber.final[[1]]$info)))
colnames(Huber.results) <- colnames(Huber.final[[1]]$info)

#initialize error vector
HuberLasso.errors <- numeric()

#fill results
for(i in 1:length(Huber.final)) {
  if(is.null(Huber.final[[i]]$error)) {
    Huber.results[i , ] <- Huber.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    Huber.results[i , 1:7] <- HD.data_DEBUG[[i]]$conditions
    HuberLasso.errors <- c(HuberLasso.errors , i)
  }
}

mean(Huber.results[ , "fpr"] , na.rm = T)
mean(Huber.results[ , "fnr"] , na.rm = T)
mean(Huber.results[!is.infinite(Huber.results[ , "mpe"]) , "mpe"] , na.rm = T)
length(HuberLasso.errors)

#saveRDS(Huber.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/Huber_resultDF_HD_500.RData")
#saveRDS(HuberLasso.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/Huber_errorindices_HD_500.RData")

