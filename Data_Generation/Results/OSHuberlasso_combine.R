#load data
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
OShuber.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/OSHuberLasso_debug.RData")

#initialize dataframe
OShuber.results <- data.frame(matrix(ncol = length(OShuber.debug[[1]]$result$important$info)))
colnames(OShuber.results) <- colnames(OShuber.debug[[1]]$result$important$info)

#fill results
for(i in 1:length(OShuber.debug)) {
  if(is.null(OShuber.debug[[i]]$error)) {
    OShuber.results[i , ] <- OShuber.debug[[i]]$result$important$info
  } else {
    cat("error at i = " , i , "\n")
    OShuber.results[i , 1:7] <- debug.data[[i]]$conditions
  }
}

mean(OShuber.results[ , "fpr"] , na.rm = T)
mean(OShuber.results[ , "fnr"] , na.rm = T)
mean(OShuber.results[ , "mpe"] , na.rm = T)
