#load data
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
elnet5.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/ELNet5_resultmain_DEBUG.RData")

#initialize dataframe
elnet5.results <- data.frame(matrix(ncol = ncol(elnet5.final[[1]]$info)))
colnames(elnet5.results) <- colnames(elnet5.final[[1]]$info)

#fill results
for(i in 1:length(elnet5.final)) {
  if(is.null(elnet5.final[[i]]$error)) {
    elnet5.results[i , ] <- elnet5.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    elnet5.results[i , 1:7] <- debug.data[[i]]$conditions
  }
}

mean(elnet5.results[ , "fpr"] , na.rm = T)
mean(elnet5.results[ , "fnr"] , na.rm = T)
mean(elnet5.results[!is.infinite(elnet5.results[ , "mpe"]) , "mpe"] , na.rm = T)

#save results
saveRDS(elnet5.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet5_resultDF_DEBUG.RData")

