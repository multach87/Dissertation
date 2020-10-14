#load data
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
msadaelnet9.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msadaelnet9_resultmain_DEBUG.RData")

#initialize dataframe
msadaelnet9.results <- data.frame(matrix(ncol = ncol(msadaelnet9.final[[1]]$info)))
colnames(msadaelnet9.results) <- colnames(msadaelnet9.final[[1]]$info)

#initialize error vector
msadaelnet9.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(msadaelnet9.final)) {
  if(is.null(msadaelnet9.final[[i]]$error)) {
    msadaelnet9.results[i , ] <- msadaelnet9.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    msadaelnet9.results[i , 1:7] <- debug.data[[i]]$conditions
    msadaelnet9.errors <- c(msadaelnet9.errors , i)
  }
}

mean(msadaelnet9.results[ , "fpr"] , na.rm = T)
mean(msadaelnet9.results[ , "fnr"] , na.rm = T)
mean(msadaelnet9.results[!is.infinite(msadaelnet9.results[ , "mpe"]) , "mpe"] , na.rm = T)

#save results
saveRDS(msadaelnet9.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msadaelnet9_resultDF_DEBUG.RData")
#saveRDS(msadaelnet9.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/msadaelnet9_errorindices_DEBUG.RData")
