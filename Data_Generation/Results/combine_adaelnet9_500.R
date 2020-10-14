#load data
half.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/500_data_10052020.RData")
adaelnet9.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet9_resultmain_500.RData")

#initialize dataframe
adaelnet9.results <- data.frame(matrix(ncol = ncol(adaelnet9.final[[1]]$info)))
colnames(adaelnet9.results) <- colnames(adaelnet9.final[[1]]$info)

#initialize error vector
adaelnet9.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(adaelnet9.final)) {
  if(is.null(adaelnet9.final[[i]]$error)) {
    adaelnet9.results[i , ] <- adaelnet9.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    adaelnet9.results[i , 1:7] <- half.data[[i]]$conditions
    adaelnet9.errors <- c(adaelnet9.errors , i)
  }
}

mean(adaelnet9.results[ , "fpr"] , na.rm = T)
mean(adaelnet9.results[ , "fnr"] , na.rm = T)
mean(adaelnet9.results[!is.infinite(adaelnet9.results[ , "mpe"]) , "mpe"] , na.rm = T)

#save results
saveRDS(adaelnet9.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet9_resultDF_500.RData")
#saveRDS(adaelnet9.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adaelnet9_errorindices_500.RData")
