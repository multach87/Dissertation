#load data
half.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/500_data_10052020.RData")
adaelnet75.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet75_resultmain_500.RData")

#initialize dataframe
adaelnet75.results <- data.frame(matrix(ncol = ncol(adaelnet75.final[[1]]$info)))
colnames(adaelnet75.results) <- colnames(adaelnet75.final[[1]]$info)

#initialize error vector
adaelnet75.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(adaelnet75.final)) {
  if(is.null(adaelnet75.final[[i]]$error)) {
    adaelnet75.results[i , ] <- adaelnet75.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    adaelnet75.results[i , 1:7] <- half.data[[i]]$conditions
    adaelnet75.errors <- c(adaelnet75.errors , i)
  }
}

mean(adaelnet75.results[ , "fpr"] , na.rm = T)
mean(adaelnet75.results[ , "fnr"] , na.rm = T)
mean(adaelnet75.results[!is.infinite(adaelnet75.results[ , "mpe"]) , "mpe"] , na.rm = T)

#save results
saveRDS(adaelnet75.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet75_resultDF_500.RData")
#saveRDS(adaelnet75.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adaelnet75_errorindices_500.RData")
