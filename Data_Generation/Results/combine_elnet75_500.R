#load data
half.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/500_data_10052020.RData")
elnet75.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet75_resultmain_500.RData")

#initialize dataframe
elnet75.results <- data.frame(matrix(ncol = ncol(elnet75.final[[1]]$info)))
colnames(elnet75.results) <- colnames(elnet75.final[[1]]$info)

#initialize error vector
elnet75.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(elnet75.final)) {
  if(is.null(elnet75.final[[i]]$error)) {
    elnet75.results[i , ] <- elnet75.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    elnet75.results[i , 1:7] <- half.data[[i]]$conditions
    elnet75.errors <- c(elnet75.errors , i)
  }
}

mean(elnet75.results[ , "fpr"] , na.rm = T)
mean(elnet75.results[ , "fnr"] , na.rm = T)
mean(elnet75.results[!is.infinite(elnet75.results[ , "mpe"]) , "mpe"] , na.rm = T)

#save results
saveRDS(elnet75.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet75_resultDF_500.RData")
#saveRDS(elnet75.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet75_errorindices_500.RData")
