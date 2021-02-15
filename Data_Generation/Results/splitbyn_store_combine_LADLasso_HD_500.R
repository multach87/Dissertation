#load data
ladlasso.HD.n200p190 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_n200p190_500.RData")
ladlasso.HD.n200p200 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_n200p200_500.RData")
ladlasso.HD.n200p210 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_n200p210_500.RData")
#ladlasso.HD.n200p500 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_n200p500_500.RData")
#ladlasso.HD.n200p1000 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_n200p1000_500.RData")
#ladlasso.HD.ex1ey0 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_ex1ey0_500.RData")
ladlasso.HD.ex2ey0 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_ex2ey0_500.RData")
#ladlasso.HD.ex0ey1 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_ex0ey1_500.RData")
#ladlasso.HD.ex1ey1 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_ex1ey1_500.RData")
#ladlasso.HD.ex2ey1 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_ex2ey1_500.RData")
#ladlasso.HD.ex0ey2 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_ex0ey2_500.RData")
#ladlasso.HD.ex1ey2 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_ex1ey2_500.RData")
#ladlasso.HD.ex2ey2 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_ex2ey2_500.RData")
#ladlasso.HD.g2h0 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_g2h0_500.RData")
#ladlasso.HD.g0h2 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_g0h2_500.RData")
#ladlasso.HD.g2h2 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_g2h2_500.RData")

#combine into one list
ladlasso.HD.all <- c(ladlasso.HD.n200p190 , ladlasso.HD.n200p200 , 
                   ladlasso.HD.n200p210)# , #ladlasso.HD.n200p500 , 
                   #ladlasso.HD.n200p1000 , ladlasso.HD.ex1ey0 , 
#                   ladlasso.HD.ex2ey0) #, ladlasso.HD.ex0ey1 , 
                   #ladlasso.HD.ex1ey1 , ladlasso.HD.ex2ey1 ,
                   #ladlasso.HD.ex0ey2 , ladlasso.HD.ex1ey2 , 
                   #ladlasso.HD.ex2ey2 , ladlasso.HD.g2h0 , 
                   #ladlasso.HD.g0h2 , ladlasso.HD.g2h2)

#clear split-by-n data from environment
#rm(list = c("ladlasso.HD.distr25" , "ladlasso.HD.distr50" , 
#            "ladlasso.HD.distr100" , "ladlasso.HD.distr200" ,
#            "ladlasso.HD.outlier25" , "ladlasso.HD.outlier50" , 
#            "ladlasso.HD.outlier100" , "ladlasso.HD.outlier200"))

#load half data
#half.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/500_data_10052020.RData")
HD.n200p190 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_n200p190.RData")
HD.n200p200 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_n200p200.RData")
HD.n200p210 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_n200p210.RData")
HD.ex2ey0 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_ex2ey0.RData")
HD.data.current <- c(HD.n200p190 , HD.n200p200 , HD.n200p210)# , HD.ex2ey0)

#dealing with error/result from map(safely())
#create empty lists for error + result
ladlasso.HD.error <- list()
ladlasso.HD.result <- list()
ladlasso.HD.final <- list()
#split data into separate error and result lists
for(i in 1:length(ladlasso.HD.all)) { 
  #iteration tracker
  #cat("i = " , i , "\n")
  #fill error list
  ladlasso.HD.error[[i]] <- list(error = ladlasso.HD.all[[i]]$error , 
                                 condition = as.data.frame(unlist(ladlasso.HD.all[[i]]$result$condition) , 
                                                           n = n , p = p , 
                                                           eta.x = eta.x , eta.y = eta.y , 
                                                           g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  ladlasso.HD.result[[i]] <- ladlasso.HD.all[[i]]$result
  #fill final list
  if(!is.null(ladlasso.HD.all[[i]]$result)) {
    ladlasso.HD.final[[i]] <- ladlasso.HD.all[[i]]$result$important
  } else {
    cat("error at i = " , i , "/n")
    ladlasso.HD.final[[i]] <- ladlasso.HD.error[[i]]
  }
}



#initialize dataframe
ladlasso.HD.maindf <- data.frame(matrix(ncol = ncol(ladlasso.HD.final[[1]]$info)))
colnames(ladlasso.HD.maindf) <- colnames(ladlasso.HD.final[[1]]$info)

#initialize error vector
ladlasso.HD.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(ladlasso.HD.final)) {
  if(is.null(ladlasso.HD.final[[i]]$error)) {
    ladlasso.HD.maindf[i , ] <- ladlasso.HD.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    ladlasso.HD.maindf[i , 1:7] <- HD.data.current[[i]]$conditions
    ladlasso.HD.errors <- c(ladlasso.HD.errors , i)
    #ladlasso.HD.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
    #ladlasso.HD.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  }
}

mean(ladlasso.HD.maindf[ , "fpr"])
mean(ladlasso.HD.maindf[ , "fpr"] , na.rm = T) #with null models included
mean(ladlasso.HD.maindf[ , "fnr"] , na.rm = T) #with null models included
mean(ladlasso.HD.maindf[ , "mpe"] , na.rm = T)
mean(ladlasso.HD.maindf[!is.infinite(ladlasso.HD.maindf[ , "mpe"]) , "mpe"] , na.rm = T) #can only calculate without null models due to error
mean(ladlasso.HD.maindf[-ladlasso.HD.errors , "fpr"] , na.rm = T) #null models excluded
mean(ladlasso.HD.maindf[-ladlasso.HD.errors , "fnr"] , na.rm = T) #null models excluded

#save results
saveRDS(ladlasso.HD.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/ladlasso_HD_result_500.RData")
saveRDS(ladlasso.HD.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/ladlasso_HD_error_500.RData")
saveRDS(ladlasso.HD.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/ladlasso_HD_resultmain_500.RData")
saveRDS(ladlasso.HD.maindf , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/ladlasso_HD_resultDF_500.RData")
#saveRDS(ladlasso.HD.errors , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/ladlasso.HD_errorindices_500.RData")
