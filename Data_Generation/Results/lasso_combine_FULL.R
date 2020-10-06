#load data
#debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
full.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/fulldata_091620.RData")
lasso.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/lasso_resultmain_FULL.RData")

#initialize dataframe
lasso.results <- data.frame(matrix(ncol = ncol(lasso.final[[1]]$info)))
colnames(lasso.results) <- colnames(lasso.final[[1]]$info)

#fill results
for(i in 1:length(lasso.final)) {
  if(is.null(lasso.final[[i]]$error)) {
    lasso.results[i , ] <- lasso.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    lasso.results[i , 1:7] <- full.data[[i]]$conditions
  }
}

mean(lasso.results[ , "fpr"] , na.rm = T)
mean(lasso.results[ , "fnr"] , na.rm = T)
mean(lasso.results[ , "mpe"] , na.rm = T)

saveRDS(lasso.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/Lasso_resultDF_FULL.RData")

