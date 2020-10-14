#load data
half.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/500_data_10052020.RData")
oshuberlasso.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/oshuberlasso_resultmain_500.RData")

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
    oshuberlasso.results[i , 1:7] <- half.data[[i]]$conditions
    oshuberlasso.errors <- c(oshuberlasso.errors , i)
  }
}

mean(oshuberlasso.results[ , "fpr"] , na.rm = T)
mean(oshuberlasso.results[ , "fnr"] , na.rm = T)
mean(oshuberlasso.results[!is.infinite(oshuberlasso.results[ , "mpe"]) , "mpe"] , na.rm = T)

#save results
saveRDS(oshuberlasso.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/oshuberlasso_resultDF_500.RData")
#saveRDS(oshuberlasso.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/oshuberlasso_errorindices_500.RData")
