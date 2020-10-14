#load full/half/debug simulated data - for filling in data condition information if there was an error
##since only the error info will be present in that part of the simulated model object
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
#half.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/500_data_10052020.RData")
#load simulation data
stdadaelnet75.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/StdAdaelnet75_resultmain_DEBUG.RData")

#initialize dataframe
stdadaelnet75.results <- data.frame(matrix(ncol = ncol(stdadaelnet75.final[[1]]$info)))
colnames(stdadaelnet75.results) <- colnames(stdadaelnet75.final[[1]]$info)

#fill results
##ERRORS AT: NONE
for(i in 1:length(stdadaelnet75.final)) {
  if(is.null(stdadaelnet75.final[[i]]$error)) {
    stdadaelnet75.results[i , ] <- stdadaelnet75.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    stdadaelnet75.results[i , 1:7] <- debug.data[[i]]$conditions
  }
}

mean(stdadaelnet75.results[ , "fpr"] , na.rm = T)
mean(stdadaelnet75.results[ , "fnr"] , na.rm = T)
mean(stdadaelnet75.results[!is.infinite(stdadaelnet75.results[ , "mpe"]) , "mpe"] , na.rm = T)


#check on error conditions NONE
#View(stdadaelnet9.results[c( ) , ])

saveRDS(stdadaelnet75.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/StdAdaelnet75_resultDF_DEBUG.RData")

