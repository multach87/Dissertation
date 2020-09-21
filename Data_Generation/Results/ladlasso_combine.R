#load data
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
ladlasso.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/ladlasso_resultmain_DEBUG.RData")

#initialize dataframe
ladlasso.results <- data.frame(matrix(ncol = ncol(ladlasso.final[[1]]$info)))
colnames(ladlasso.results) <- colnames(ladlasso.final[[1]]$info)

#fill results
for(i in 1:length(ladlasso.final)) {
  if(is.null(ladlasso.final[[i]]$error)) {
    ladlasso.results[i , ] <- ladlasso.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    ladlasso.results[i , 1:7] <- debug.data[[i]]$conditions
  }
}

mean(ladlasso.results[ , "fpr"] , na.rm = T)
mean(ladlasso.results[ , "fnr"] , na.rm = T)
mean(ladlasso.results[ , "mpe"] , na.rm = T)
