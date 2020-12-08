#load data
msaelnet5.outlier25 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_outlier25_500.RData")
msaelnet5.outlier50 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_outlier50_500.RData")
msaelnet5.outlier100 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_outlier100_500.RData")
msaelnet5.outlier200 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_outlier200_500.RData")
msaelnet5.distr25 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_distr25_500.RData")
msaelnet5.distr50 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_distr50_500.RData")
msaelnet5.distr100 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_distr100_500.RData")
msaelnet5.distr200 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/msaelnet5_distr200_500.RData")

#combine into one list
msaelnet5.all <- c(msaelnet5.outlier25 , msaelnet5.outlier50 , 
                   msaelnet5.outlier100 , msaelnet5.outlier200 , 
                   msaelnet5.distr25 , msaelnet5.distr50 , 
                   msaelnet5.distr100 , msaelnet5.distr200)

#clear split-by-n data from environment
rm(list = c("msaelnet5.distr25" , "msaelnet5.distr50" , 
            "msaelnet5.distr100" , "msaelnet5.distr200" ,
            "msaelnet5.outlier25" , "msaelnet5.outlier50" , 
            "msaelnet5.outlier100" , "msaelnet5.outlier200"))

#load half data
half.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/500_data_10052020.RData")


#dealing with error/result from map(safely())
#create empty lists for error + result
msaelnet5.error <- list()
msaelnet5.result <- list()
msaelnet5.final <- list()
#split data into separate error and result lists
for(i in 1:length(msaelnet5.all)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  msaelnet5.error[[i]] <- list(error = msaelnet5.all[[i]]$error , 
                                 condition = as.data.frame(unlist(msaelnet5.all[[i]]$result$condition) , 
                                                           n = n , p = p , 
                                                           eta.x = eta.x , eta.y = eta.y , 
                                                           g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  msaelnet5.result[[i]] <- msaelnet5.all[[i]]$result
  #fill final list
  if(!is.null(msaelnet5.all[[i]]$result)) {
    msaelnet5.final[[i]] <- msaelnet5.all[[i]]$result$important
  } else {
    msaelnet5.final[[i]] <- msaelnet5.error[[i]]
  }
}



#initialize dataframe
msaelnet5.maindf <- data.frame(matrix(ncol = ncol(msaelnet5.final[[1]]$info)))
colnames(msaelnet5.maindf) <- colnames(msaelnet5.final[[1]]$info)

#initialize error vector
msaelnet5.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(msaelnet5.final)) {
  if(is.null(msaelnet5.final[[i]]$error)) {
    msaelnet5.maindf[i , ] <- msaelnet5.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    msaelnet5.maindf[i , 1:7] <- half.data[[i]]$conditions
    msaelnet5.errors <- c(msaelnet5.errors , i)
    msaelnet5.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
    msaelnet5.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  }
}

mean(msaelnet5.maindf[ , "fpr"] , na.rm = T) #with null models included
mean(msaelnet5.maindf[ , "fnr"] , na.rm = T) #with null models included
mean(msaelnet5.maindf[!is.infinite(msaelnet5.maindf[ , "mpe"]) , "mpe"] , na.rm = T) #can only calculate without null models due to error
mean(msaelnet5.maindf[-msaelnet5.errors , "fpr"] , na.rm = T) #null models excluded
mean(msaelnet5.maindf[-msaelnet5.errors , "fnr"] , na.rm = T) #null models excluded

#save results
saveRDS(msadaelnet5.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/msadaelnet5_result_500.RData")
saveRDS(msadaelnet5.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/msadaelnet5_error_500.RData")
saveRDS(msadaelnet5.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msadaelnet5_resultmain_500.RData")
saveRDS(msaelnet5.maindf , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msaelnet5_resultDF_500.RData")
#saveRDS(msaelnet5.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/msaelnet5_errorindices_500.RData")
