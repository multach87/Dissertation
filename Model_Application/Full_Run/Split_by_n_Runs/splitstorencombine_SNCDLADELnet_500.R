#load data
#load packages
library(hqreg)
library(glmnet)
library(magrittr)
library(purrr)

#load simulated data
#outlier100.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/outlier100.RData")
#outlier200.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/outlier200.RData")
#distr25.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/distr25.RData")
#distr50.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/distr50.RData")
distr100.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/distr100.RData")
distr200.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/distr200.RData")
data.current <- c(#outlier100.data , outlier200.data , distr25.data , distr50.data , 
  distr200.data) # ,  distr200.data)

#load model data
#SCNDLADelnet.outlier100 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SCNDLADelnet_outlier100_500.RData")
#SCNDLADelnet.outlier200 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SCNDLADelnet_outlier200_500.RData")
#SCNDLADelnet.distr25 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SCNDLADelnet_distr25_500.RData")
#SCNDLADelnet.distr50 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SCNDLADelnet_distr50_500.RData")
#SCNDLADelnet.distr100 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/")
SNCDLADelnet.distr200 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADelnet_distr200_500.RData")

#combine split results
SCNDLADelnet.all <- SNCDLADelnet.distr200 #c(SCNDLADelnet.outlier100 , SCNDLADelnet.outlier200 , SCNDLADelnet.distr25 , SCNDLADelnet.distr50 , SCNDLADelnet.distr100 , SCNDLADelnet.distr200)
#SCNDLADelnet.all <- c()


#dealing with error/result from map(safely())
#create empty lists for error + result
SCNDLADelnet.error <- list()
SCNDLADelnet.result <- list()
SCNDLADelnet.final <- list()
#split data into separate error and result lists
for(i in 1:length(SCNDLADelnet.all)) { 
  #iteration tracker
  #cat("i = " , i , "\n")
  #fill error list
  SCNDLADelnet.error[[i]] <- list(error = SCNDLADelnet.all[[i]]$error , 
                                 condition = as.data.frame(unlist(SCNDLADelnet.all[[i]]$result$condition) , 
                                                           n = n , p = p , 
                                                           eta.x = eta.x , eta.y = eta.y , 
                                                           g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  SCNDLADelnet.result[[i]] <- SCNDLADelnet.all[[i]]$result
  #fill final list
  if(!is.null(SCNDLADelnet.all[[i]]$result)) {
    SCNDLADelnet.final[[i]] <- SCNDLADelnet.all[[i]]$result$important
  } else {
    cat("error at i = " , i , "/n")
    SCNDLADelnet.final[[i]] <- SCNDLADelnet.error[[i]]
  }
}



#initialize dataframe
SCNDLADelnet.maindf <- data.frame(matrix(ncol = ncol(SCNDLADelnet.final[[1]])))
colnames(SCNDLADelnet.maindf) <- colnames(SCNDLADelnet.final[[1]])

#initialize error vector
SCNDLADelnet.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(SCNDLADelnet.final)) {
  if(is.null(SCNDLADelnet.final[[i]]$error)) {
    SCNDLADelnet.maindf[i , ] <- SCNDLADelnet.final[[i]]
  } else {
    cat("error at i = " , i , "\n")
    SCNDLADelnet.maindf[i , 1:7] <- HD.data.current[[i]]$conditions
    SCNDLADelnet.errors <- c(SCNDLADelnet.errors , i)
    #SCNDLADelnet.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
    #SCNDLADelnet.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  }
}


#Check results
mean(SCNDLADelnet.maindf[ , "fpr"])
mean(SCNDLADelnet.maindf[ , "fpr"] , na.rm = T) #with null models included
mean(SCNDLADelnet.maindf[ , "fnr"])
mean(SCNDLADelnet.maindf[ , "fnr"] , na.rm = T) #with null models included
mean(SCNDLADelnet.maindf[ , "mpe"])
mean(SCNDLADelnet.maindf[ , "mpe"] , na.rm = T)
mean(SCNDLADelnet.maindf[!is.infinite(SCNDLADelnet.maindf[ , "mpe"]) , "mpe"])
