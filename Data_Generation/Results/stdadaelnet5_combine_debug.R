#load full/half/debug simulated data - for filling in data condition information if there was an error
##since only the error info will be present in that part of the simulated model object
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
#half.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/500_data_10052020.RData")
#load simulation data
stdadaelnet5.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/StdAdaELNet5_resultmain_DEBUG.RData")

#initialize dataframe
stdadaelnet5.results <- data.frame(matrix(ncol = ncol(stdadaelnet5.final[[1]]$info)))
colnames(stdadaelnet5.results) <- colnames(stdadaelnet5.final[[1]]$info)

#fill results
##ERRORS AT: 
for(i in 1:length(stdadaelnet5.final)) {
  if(is.null(stdadaelnet5.final[[i]]$error)) {
    stdadaelnet5.results[i , ] <- stdadaelnet5.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    stdadaelnet5.results[i , 1:7] <- debug.data[[i]]$conditions
  }
}

mean(stdadaelnet5.results[ , "fpr"] , na.rm = T)
mean(stdadaelnet5.results[ , "fnr"] , na.rm = T)
mean(stdadaelnet5.results[!is.infinite(stdadaelnet5.results[ , "mpe"]) , "mpe"] , na.rm = T)


#check on error conditions NONE
#View(stdadaelnet5.results[c( ) , ])

saveRDS(stdadaelnet5.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/StdAdaELNet5_resultDF_DEBUG.RData")

