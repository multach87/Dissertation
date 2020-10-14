#load data
half.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/500_data_10052020.RData")
ladlasso.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/ladlasso_resultmain_500.RData")

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
    ladlasso.results[i , 1:7] <- half.data[[i]]$conditions
    ladlasso.errors <- c(ladlasso.errors , i)
  }
}

mean(ladlasso.results[ , "fpr"] , na.rm = T)
mean(ladlasso.results[ , "fnr"] , na.rm = T)
mean(ladlasso.results[!is.infinite(ladlasso.results[ , "mpe"]) , "mpe"] , na.rm = T)

#save results
saveRDS(ladlasso.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/ladlasso_resultDF_500.RData")
saveRDS(ladlasso.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/ladlasso_errorindices_500.RData")
