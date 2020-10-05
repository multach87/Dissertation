#load data
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
elnet9.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet9_resultmain_DEBUG.RData")

#initialize dataframe
elnet9.results <- data.frame(matrix(ncol = ncol(elnet9.final[[1]]$info)))
colnames(elnet9.results) <- colnames(elnet9.final[[1]]$info)

#fill results
for(i in 1:length(elnet9.final)) {
  if(is.null(elnet9.final[[i]]$error)) {
    elnet9.results[i , ] <- elnet9.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    elnet9.results[i , 1:7] <- debug.data[[i]]$conditions
  }
}

mean(elnet9.results[ , "fpr"] , na.rm = T)
mean(elnet9.results[ , "fnr"] , na.rm = T)
mean(elnet9.results[ , "mpe"] , na.rm = T)

