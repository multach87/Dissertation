#load data
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
msadaelnet75.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msadaelnet75_resultmain_DEBUG.RData")

#initialize dataframe
msadaelnet75.results <- data.frame(matrix(ncol = ncol(msadaelnet75.final[[1]]$info)))
colnames(msadaelnet75.results) <- colnames(msadaelnet75.final[[1]]$info)

#initialize error vector
msadaelnet75.errors <- numeric()

#fill results
##Errors at: c(32 , 65 , 157)
for(i in 1:length(msadaelnet75.final)) {
  if(is.null(msadaelnet75.final[[i]]$error)) {
    msadaelnet75.results[i , ] <- msadaelnet75.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    msadaelnet75.results[i , 1:7] <- debug.data[[i]]$conditions
    msadaelnet75.errors <- c(msadaelnet75.errors , i)
  }
}

mean(msadaelnet75.results[ , "fpr"] , na.rm = T)
mean(msadaelnet75.results[ , "fnr"] , na.rm = T)
mean(msadaelnet75.results[!is.infinite(msadaelnet75.results[ , "mpe"]) , "mpe"] , na.rm = T)

#save results
saveRDS(msadaelnet75.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msadaelnet75_resultDF_DEBUG.RData")
#saveRDS(msadaelnet75.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/msadaelnet75_errorindices_DEBUG.RData")
