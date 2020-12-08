#load data
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
msadaelnet5.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msadaelnet5_resultmain_DEBUG.RData")

#initialize dataframe
msadaelnet5.results <- data.frame(matrix(ncol = ncol(msadaelnet5.final[[1]]$info)))
colnames(msadaelnet5.results) <- colnames(msadaelnet5.final[[1]]$info)

#initialize error vector
msadaelnet5.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(msadaelnet5.final)) {
  if(is.null(msadaelnet5.final[[i]]$error)) {
    msadaelnet5.results[i , ] <- msadaelnet5.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    msadaelnet5.results[i , 1:7] <- debug.data[[i]]$conditions
    msadaelnet5.errors <- c(msadaelnet5.errors , i)
  }
}

mean(msadaelnet5.results[ , "fpr"] , na.rm = T)
mean(msadaelnet5.results[ , "fnr"] , na.rm = T)
mean(msadaelnet5.results[!is.infinite(msadaelnet5.results[ , "mpe"]) , "mpe"] , na.rm = T)

#save results
saveRDS(msadaelnet5.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msadaelnet5_resultDF_DEBUG.RData")
#saveRDS(msadaelnet5.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/msadaelnet5_errorindices_DEBUG.RData")
