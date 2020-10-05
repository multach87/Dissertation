#load data
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
elnet75.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet75_resultmain_DEBUG.RData")

#initialize dataframe
elnet75.results <- data.frame(matrix(ncol = ncol(elnet75.final[[1]]$info)))
colnames(elnet75.results) <- colnames(elnet75.final[[1]]$info)

#fill results
for(i in 1:length(elnet75.final)) {
  if(is.null(elnet75.final[[i]]$error)) {
    elnet75.results[i , ] <- elnet75.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    elnet75.results[i , 1:7] <- debug.data[[i]]$conditions
  }
}

mean(elnet75.results[ , "fpr"] , na.rm = T)
mean(elnet75.results[ , "fnr"] , na.rm = T)
mean(elnet75.results[ , "mpe"] , na.rm = T)

