#load data
half.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/500_data_10052020.RData")
elnet9.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet9_resultmain_500.RData")

#initialize dataframe
elnet9.results <- data.frame(matrix(ncol = ncol(elnet9.final[[1]]$info)))
colnames(elnet9.results) <- colnames(elnet9.final[[1]]$info)

#initialize error vector
elnet9.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(elnet9.final)) {
  if(is.null(elnet9.final[[i]]$error)) {
    elnet9.results[i , ] <- elnet9.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    elnet9.results[i , 1:7] <- half.data[[i]]$conditions
    elnet9.errors <- c(elnet9.errors , i)
  }
}

mean(elnet9.results[ , "fpr"] , na.rm = T)
mean(elnet9.results[ , "fnr"] , na.rm = T)
mean(elnet9.results[!is.infinite(elnet9.results[ , "mpe"]) , "mpe"] , na.rm = T)

#save results
saveRDS(elnet9.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet9_resultDF_500.RData")
#saveRDS(elnet9.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet9_errorindices_500.RData")
