#load data
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
elnet9.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet9_debug.RData")

#initialize dataframe
elnet9.results <- data.frame(matrix(ncol = ncol(elnet9.debug[[1]]$info)))
colnames(elnet9.results) <- colnames(elnet9.debug[[1]]$info)

#fill results
for(i in 1:length(elnet9.debug)) {
  if(is.null(elnet9.debug[[i]]$error)) {
    elnet9.results[i , ] <- elnet9.debug[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    elnet9.results[i , 1:7] <- debug.data[[i]]$conditions
  }
}

mean(elnet9.results[ , "fpr"] , na.rm = T)
mean(elnet9.results[ , "fnr"] , na.rm = T)
mean(elnet9.results[ , "mpe"] , na.rm = T)
