#load data
HD.halfdata <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HDSparsedata_112320.RData")

OSHuberLasso.HD.HALF <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/OSHuberLasso_HD_500.RData")


#split and store results
#dealing with error/result from map(safely())
#create empty lists for error + result
oshuberlasso.error <- list()
oshuberlasso.result <- list()
oshuberlasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(OSHuberLasso.HD.HALF)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  oshuberlasso.error[[i]] <- list(error = OSHuberLasso.HD.HALF[[i]]$error , 
                                  condition = as.data.frame(unlist(OSHuberLasso.HD.HALF[[i]]$condition) , 
                                                            n = n , p = p , 
                                                            eta.x = eta.x , eta.y = eta.y , 
                                                            g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  oshuberlasso.result[[i]] <- OSHuberLasso.HD.HALF[[i]]$result
  #fill final list
  if(!is.null(OSHuberLasso.HD.HALF[[i]]$result)) {
    oshuberlasso.final[[i]] <- OSHuberLasso.HD.HALF[[i]]$result$important
  } else {
    oshuberlasso.final[[i]] <- oshuberlasso.error[[i]]
  }
}


#save files
#saveRDS(oshuberlasso.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/oshuberlasso_result_HD_500.RData")
#saveRDS(oshuberlasso.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/oshuberlasso_error_HD_500.RData")
#saveRDS(oshuberlasso.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/oshuberlasso_resultmain_HD_500.RData")





#combine results
#initialize dataframe
oshuberlasso.results <- data.frame(matrix(ncol = ncol(oshuberlasso.final[[1]]$info)))
colnames(oshuberlasso.results) <- colnames(oshuberlasso.final[[1]]$info)

#initialize error vector
oshuberlasso.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(oshuberlasso.final)) {
  if(is.null(oshuberlasso.final[[i]]$error)) {
    oshuberlasso.results[i , ] <- oshuberlasso.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    oshuberlasso.results[i , 1:7] <- HD.halfdata[[i]]$conditions
    oshuberlasso.errors <- c(oshuberlasso.errors , i)
  }
}

mean(oshuberlasso.results[ , "fpr"] , na.rm = T)
mean(oshuberlasso.results[ , "fnr"] , na.rm = T)
mean(oshuberlasso.results[!is.infinite(oshuberlasso.results[ , "mpe"]) , "mpe"] , na.rm = T)
length(oshuberlasso.errors)

#save results
saveRDS(oshuberlasso.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/oshuberlasso_resultDF_HD_500.RData")
saveRDS(oshuberlasso.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/oshuberlasso_errorindices_HD_500.RData")
