#load data
pense5.outlier25 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/pense5_outlier25.RData")
pense5.outlier50 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/pense5_outlier50.RData")
pense5.outlier100 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/pense5_outlier100.RData")
#pense5.outlier200 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/pense5_outlier200.RData")
pense5.distr25 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/pense5_distr25.RData")
pense5.distr50 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/pense5_distr50.RData")
pense5.distr100 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/pense5_distr100.RData")
pense5.distr200 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/pense5_distr200.RData")

#combine into one list
pense5.all <- c(pense5.outlier25 , pense5.outlier50 , 
                   pense5.outlier100 , #pense5.outlier200 , 
                   pense5.distr25 , pense5.distr50 , 
                   pense5.distr100 , pense5.distr200)

#clear split-by-n data from environment
rm(list = c("pense5.distr25" , "pense5.distr50" , 
            "pense5.distr100" , "pense5.distr200" ,
            "pense5.outlier25" , "pense5.outlier50" , 
            "pense5.outlier100")) # , "pense5.outlier200"))

#load half data
half.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/500_data_10052020.RData")
half.data.incomplete <- half.data[!27001:36000]

#dealing with error/result from map(safely())
#create empty lists for error + result
pense5.error <- list()
pense5.result <- list()
pense5.final <- list()
#split data into separate error and result lists
for(i in 1:length(pense5.all)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  pense5.error[[i]] <- list(error = pense5.all[[i]]$error , 
                                 condition = as.data.frame(unlist(pense5.all[[i]]$result$condition) , 
                                                           n = n , p = p , 
                                                           eta.x = eta.x , eta.y = eta.y , 
                                                           g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  pense5.result[[i]] <- pense5.all[[i]]$result
  #fill final list
  if(!is.null(pense5.all[[i]]$result)) {
    pense5.final[[i]] <- pense5.all[[i]]$result$important
  } else {
    pense5.final[[i]] <- pense5.error[[i]]
  }
}



#initialize dataframe
pense5.maindf <- data.frame(matrix(ncol = ncol(pense5.final[[1]]$info)))
colnames(pense5.maindf) <- colnames(pense5.final[[1]]$info)

#initialize error vector
pense5.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(pense5.final)) {
  if(is.null(pense5.final[[i]]$error)) {
    pense5.maindf[i , ] <- pense5.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    pense5.maindf[i , 1:7] <- half.data[[i]]$conditions
    pense5.errors <- c(pense5.errors , i)
    pense5.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
    pense5.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  }
}

mean(pense5.maindf[ , "fpr"] , na.rm = T) #with null models included
mean(pense5.maindf[ , "fnr"] , na.rm = T) #with null models included
mean(pense5.maindf[!is.infinite(pense5.maindf[ , "mpe"]) , "mpe"] , na.rm = T) #can only calculate without null models due to error
mean(pense5.maindf[-pense5.errors , "fpr"] , na.rm = T) #null models excluded
mean(pense5.maindf[-pense5.errors , "fnr"] , na.rm = T) #null models excluded

#save results
saveRDS(pense5.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/pense5_result_500.RData")
saveRDS(pense5.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/pense5_error_500.RData")
saveRDS(pense5.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/pense5_resultmain_500.RData")
saveRDS(pense5.maindf , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/pense5_resultDF_500.RData")
#saveRDS(pense5.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/pense5_errorindices_500.RData")
