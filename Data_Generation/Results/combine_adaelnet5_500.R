#load data
half.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/500_data_10052020.RData")
adaelnet5.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet5_resultmain_500.RData")

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
    adaelnet5.results[i , 1:7] <- half.data[[i]]$conditions
    adaelnet5.errors <- c(adaelnet5.errors , i)
  }
}

mean(adaelnet5.results[ , "fpr"] , na.rm = T)
mean(adaelnet5.results[ , "fnr"] , na.rm = T)
mean(adaelnet5.results[!is.infinite(adaelnet5.results[ , "mpe"]) , "mpe"] , na.rm = T)

#save results
saveRDS(adaelnet5.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet5_resultDF_500.RData")
#saveRDS(adaelnet5.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adaelnet5_errorindices_500.RData")
