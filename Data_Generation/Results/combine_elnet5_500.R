#load data
half.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/500_data_10052020.RData")
elnet5.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet5_resultmain_500.RData")

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
    elnet5.results[i , 1:7] <- half.data[[i]]$conditions
    elnet5.errors <- c(elnet5.errors , i)
  }
}

mean(elnet5.results[ , "fpr"] , na.rm = T)
mean(elnet5.results[ , "fnr"] , na.rm = T)
mean(elnet5.results[!is.infinite(elnet5.results[ , "mpe"]) , "mpe"] , na.rm = T)

#save results
saveRDS(elnet5.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet5_resultDF_500.RData")
#saveRDS(elnet5.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet5_errorindices_500.RData")
