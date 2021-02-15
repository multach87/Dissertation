#load data
HD.halfdata <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HDSparsedata_112320.RData")


adaelnet5.HD.HALF <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/adaelnet5_HD_500.RData")









#split and store results
#dealing with error/result from map(safely())
#create empty lists for error + result
adaelnet5.error <- list()
adaelnet5.result <- list()
adaelnet5.final <- list()
#split data into separate error and result lists
for(i in 1:length(adaelnet5.HD.HALF)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  adaelnet5.error[[i]] <- list(error = adaelnet5.HD.HALF[[i]]$error , 
                               condition = as.data.frame(unlist(adaelnet5.HD.HALF[[i]]$result$condition) , 
                                                         n = n , p = p , 
                                                         eta.x = eta.x , eta.y = eta.y , 
                                                         g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  adaelnet5.result[[i]] <- adaelnet5.HD.HALF[[i]]$result
  #fill final list
  if(!is.null(adaelnet5.HD.HALF[[i]]$result)) {
    adaelnet5.final[[i]] <- adaelnet5.HD.HALF[[i]]$result$important
  } else {
    adaelnet5.final[[i]] <- adaelnet5.error[[i]]
  }
}


#save files
#saveRDS(adaelnet5.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/adaelnet5_result_HD_500.RData")
#saveRDS(adaelnet5.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adaelnet5_error_HD_500.RData")
#saveRDS(adaelnet5.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet5_resultmain_HD_500.RData")



#combine results
#initialize dataframe
adaelnet5.results <- data.frame(matrix(ncol = ncol(adaelnet5.final[[1]]$info)))
colnames(adaelnet5.results) <- colnames(adaelnet5.final[[1]]$info)

#initialize error vector
adaelnet5.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(adaelnet5.final)) {
  if(is.null(adaelnet5.final[[i]]$error)) {
    adaelnet5.results[i , ] <- adaelnet5.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    adaelnet5.results[i , 1:7] <- HD.halfdata[[i]]$conditions
    adaelnet5.errors <- c(adaelnet5.errors , i)
  }
}

mean(adaelnet5.results[ , "fpr"] , na.rm = T)
mean(adaelnet5.results[ , "fnr"] , na.rm = T)
mean(adaelnet5.results[!is.infinite(adaelnet5.results[ , "mpe"]) , "mpe"] , na.rm = T)
length(adaelnet5.errors)

#save results
saveRDS(adaelnet5.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet5_resultDF_HD_500.RData")
saveRDS(adaelnet5.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adaelnet5_errorindices_HD_500.RData")
