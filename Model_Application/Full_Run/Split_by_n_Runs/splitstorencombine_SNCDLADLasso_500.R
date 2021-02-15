#load data
#load packages
library(hqreg)
library(glmnet)
library(magrittr)
library(purrr)

#load simulated data
outlier100.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/outlier100.RData")
outlier200.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/outlier200.RData")
distr25.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/distr25.RData")
distr50.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/distr50.RData")
distr100.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/distr100.RData")
distr200.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/distr200.RData")
data.current <- c(outlier100.data , outlier200.data , distr25.data , distr50.data , distr100.data ,  distr200.data)

#load model data
SNCDLADLasso.outlier100 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADLasso_outlier100_500.RData")
SNCDLADLasso.outlier200 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADLasso_outlier200_500.RData")
SNCDLADLasso.distr25 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADLasso_distr25_500.RData")
SNCDLADLasso.distr50 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADLasso_distr50_500.RData")
SNCDLADLasso.distr100 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADLasso_distr100_500.RData")
SNCDLADLasso.distr200 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADLasso_distr200_500.RData")

#combine split results
SNCDLADLasso.all <- c(SNCDLADLasso.outlier100 , SNCDLADLasso.outlier200 , SNCDLADLasso.distr25 , SNCDLADLasso.distr50 , SNCDLADLasso.distr100 , SNCDLADLasso.distr200)
#SNCDLADLasso.all <- c()


#dealing with error/result from map(safely())
#create empty lists for error + result
SNCDLADLasso.error <- list()
SNCDLADLasso.result <- list()
SNCDLADLasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(SNCDLADLasso.all)) { 
  #iteration tracker
  #cat("i = " , i , "\n")
  #fill error list
  SNCDLADLasso.error[[i]] <- list(error = SNCDLADLasso.all[[i]]$error , 
                                 condition = as.data.frame(unlist(SNCDLADLasso.all[[i]]$result$condition) , 
                                                           n = n , p = p , 
                                                           eta.x = eta.x , eta.y = eta.y , 
                                                           g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  SNCDLADLasso.result[[i]] <- SNCDLADLasso.all[[i]]$result
  #fill final list
  if(!is.null(SNCDLADLasso.all[[i]]$result)) {
    SNCDLADLasso.final[[i]] <- SNCDLADLasso.all[[i]]$result$important
  } else {
    cat("error at i = " , i , "/n")
    SNCDLADLasso.final[[i]] <- SNCDLADLasso.error[[i]]
  }
}



#initialize dataframe
SNCDLADLasso.maindf <- data.frame(matrix(ncol = ncol(SNCDLADLasso.final[[1]])))
colnames(SNCDLADLasso.maindf) <- colnames(SNCDLADLasso.final[[1]])

#initialize error vector
SNCDLADLasso.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(SNCDLADLasso.final)) {
  if(is.null(SNCDLADLasso.final[[i]]$error)) {
    SNCDLADLasso.maindf[i , ] <- SNCDLADLasso.final[[i]]
  } else {
    cat("error at i = " , i , "\n")
    SNCDLADLasso.maindf[i , 1:7] <- HD.data.current[[i]]$conditions
    SNCDLADLasso.errors <- c(SNCDLADLasso.errors , i)
    #SNCDLADLasso.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
    #SNCDLADLasso.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  }
}


#Check results
mean(SNCDLADLasso.maindf[ , "fpr"])
mean(SNCDLADLasso.maindf[ , "fpr"] , na.rm = T) #with null models included
mean(SNCDLADLasso.maindf[ , "fnr"])
mean(SNCDLADLasso.maindf[ , "fnr"] , na.rm = T) #with null models included
mean(SNCDLADLasso.maindf[ , "mpe"])
mean(SNCDLADLasso.maindf[ , "mpe"] , na.rm = T)
mean(SNCDLADLasso.maindf[!is.infinite(SNCDLADLasso.maindf[ , "mpe"]) , "mpe"])
