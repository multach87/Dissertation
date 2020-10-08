#load full/half/debug simulated data - for filling in data condition information if there was an error
##since only the error info will be present in that part of the simulated model object
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
#half.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/500_data_10052020.RData")
#load simulation data
stdadaelnet9.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/StdAdaelnet9_resultmain_DEBUG.RData")

#initialize dataframe
stdadaelnet9.results <- data.frame(matrix(ncol = ncol(stdadaelnet9.final[[1]]$info)))
colnames(stdadaelnet9.results) <- colnames(stdadaelnet9.final[[1]]$info)

#fill results
##ERRORS AT: NONE
for(i in 1:length(stdadaelnet9.final)) {
  if(is.null(stdadaelnet9.final[[i]]$error)) {
    stdadaelnet9.results[i , ] <- stdadaelnet9.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    stdadaelnet9.results[i , 1:7] <- debug.data[[i]]$conditions
  }
}

mean(stdadaelnet9.results[ , "fpr"] , na.rm = T)
mean(stdadaelnet9.results[ , "fnr"] , na.rm = T)
mean(stdadaelnet9.results[!is.infinite(stdadaelnet9.results[ , "mpe"]) , "mpe"] , na.rm = T)


#check on error conditions NONE
#View(stdadaelnet9.results[c( ) , ])

saveRDS(stdadaelnet9.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/StdAdaelnet9_resultDF_DEBUG.RData")

