#load data
#debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
full.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/fulldata_091620.RData")
Huber.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/Huber_resultmain_FULL.RData")

#initialize dataframe
Huber.results <- data.frame(matrix(ncol = ncol(Huber.final[[1]]$info)))
colnames(Huber.results) <- colnames(Huber.final[[1]]$info)

#fill results
for(i in 1:length(Huber.final)) {
  if(is.null(Huber.final[[i]]$error)) {
    Huber.results[i , ] <- Huber.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    Huber.results[i , 1:7] <- full.data[[i]]$conditions
  }
}

mean(Huber.results[ , "fpr"] , na.rm = T)
mean(Huber.results[ , "fnr"] , na.rm = T)
mean(Huber.results[ , "mpe"] , na.rm = T)

saveRDS(Huber.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/Huber_resultDF_FULL.RData")
