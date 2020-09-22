#load data
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
OS.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/OS_resultmain_DEBUG.RData")

#initialize dataframe
OS.results <- data.frame(matrix(ncol = ncol(OS.final[[1]]$info)))
colnames(OS.results) <- colnames(OS.final[[1]]$info)

#fill results
for(i in 1:length(OS.final)) {
  if(is.null(OS.final[[i]]$error)) {
    OS.results[i , ] <- OS.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    OS.results[i , 1:7] <- debug.data[[i]]$conditions
  }
}

mean(OS.results[ , "fpr"] , na.rm = T)
mean(OS.results[ , "fnr"] , na.rm = T)
mean(OS.results[ , "mpe"] , na.rm = T)
