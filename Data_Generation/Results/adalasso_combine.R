#load data
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
adalasso.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adalasso_resultmain_DEBUG.RData")

#initialize dataframe
adalasso.results <- data.frame(matrix(ncol = ncol(adalasso.final[[1]]$info)))
colnames(adalasso.results) <- colnames(adalasso.final[[1]]$info)

#fill results
for(i in 1:length(adalasso.final)) {
  if(is.null(adalasso.final[[i]]$error)) {
    adalasso.results[i , ] <- adalasso.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    adalasso.results[i , 1:7] <- debug.data[[i]]$conditions
  }
}

mean(adalasso.results[ , "fpr"] , na.rm = T)
mean(adalasso.results[ , "fnr"] , na.rm = T)
mean(adalasso.results[ , "mpe"] , na.rm = T)
