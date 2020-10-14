#load data
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
OShuber.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/OSHuber_resultmain_DEBUG.RData")

#initialize dataframe
OShuber.results <- data.frame(matrix(ncol = ncol(OShuber.final[[1]]$info)))
colnames(OShuber.results) <- colnames(OShuber.final[[1]]$info)

#fill results
for(i in 1:length(OShuber.final)) {
  if(is.null(OShuber.final[[i]]$error)) {
    OShuber.results[i , ] <- OShuber.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    OShuber.results[i , 1:7] <- debug.data[[i]]$conditions
  }
}

mean(OShuber.results[ , "fpr"] , na.rm = T)
mean(OShuber.results[ , "fnr"] , na.rm = T)
mean(OShuber.results[ , "mpe"] , na.rm = T)

