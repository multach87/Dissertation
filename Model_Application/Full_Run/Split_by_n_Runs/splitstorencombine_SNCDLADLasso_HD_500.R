#load data
#load packages
library(hqreg)
library(glmnet)
library(magrittr)
library(purrr)

#load simulated data
n200p190.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_n200p190.RData")
n200p200.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_n200p200.RData")

#load model data
SNCDLADLasso.n200p190 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADLasso_HD_n200p190_500.RData")
SNCDLADLasso.n200p200 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADLasso_HD_n200p200_500.RData")

#combine simulated data
HD.data.current <- c(n200p190.data , n200p200.data)

#combine split results
SNCDLADLasso.HD.all <- c(SNCDLADLasso.n200p190 , SNCDLADLasso.n200p200)
#SNCDLADLasso.HD.all <- c()


#dealing with error/result from map(safely())
#create empty lists for error + result
SNCDLADLasso.HD.error <- list()
SNCDLADLasso.HD.result <- list()
SNCDLADLasso.HD.final <- list()
#split data into separate error and result lists
for(i in 1:length(SNCDLADLasso.HD.all)) { 
  #iteration tracker
  #cat("i = " , i , "\n")
  #fill error list
  SNCDLADLasso.HD.error[[i]] <- list(error = SNCDLADLasso.HD.all[[i]]$error , 
                                  condition = as.data.frame(unlist(SNCDLADLasso.HD.all[[i]]$result$condition) , 
                                                            n = n , p = p , 
                                                            eta.x = eta.x , eta.y = eta.y , 
                                                            g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  SNCDLADLasso.HD.result[[i]] <- SNCDLADLasso.HD.all[[i]]$result
  #fill final list
  if(!is.null(SNCDLADLasso.HD.all[[i]]$result)) {
    SNCDLADLasso.HD.final[[i]] <- SNCDLADLasso.HD.all[[i]]$result$important
  } else {
    cat("error at i = " , i , "/n")
    SNCDLADLasso.HD.final[[i]] <- SNCDLADLasso.HD.error[[i]]
  }
}



#initialize dataframe
SNCDLADLasso.HD.maindf <- data.frame(matrix(ncol = ncol(SNCDLADLasso.HD.final[[1]])))
colnames(SNCDLADLasso.HD.maindf) <- colnames(SNCDLADLasso.HD.final[[1]])

#initialize error vector
SNCDLADLasso.HD.errors <- numeric()

#fill results
##Errors at:
for(i in 1:length(SNCDLADLasso.HD.final)) {
  if(is.null(SNCDLADLasso.HD.final[[i]]$error)) {
    SNCDLADLasso.HD.maindf[i , ] <- SNCDLADLasso.HD.final[[i]]
  } else {
    cat("error at i = " , i , "\n")
    SNCDLADLasso.HD.maindf[i , 1:7] <- HD.data.current[[i]]$conditions
    SNCDLADLasso.HD.errors <- c(SNCDLADLasso.HD.errors , i)
    #SNCDLADLasso.HD.maindf[i , "fpr"] <- 0 #set fpr = 0 for null models ("errors" when running)
    #SNCDLADLasso.HD.maindf[i , "fnr"] <- 1 #set fnr = 1 for null models ("errors" when running)
  }
}


#Check results
mean(SNCDLADLasso.HD.maindf[ , "fpr"])
mean(SNCDLADLasso.HD.maindf[ , "fpr"] , na.rm = T) #with null models included
mean(SNCDLADLasso.HD.maindf[ , "fnr"])
mean(SNCDLADLasso.HD.maindf[ , "fnr"] , na.rm = T) #with null models included
mean(SNCDLADLasso.HD.maindf[ , "mpe"])
mean(SNCDLADLasso.HD.maindf[ , "mpe"] , na.rm = T)
mean(SNCDLADLasso.HD.maindf[!is.infinite(SNCDLADLasso.HD.maindf[ , "mpe"]) , "mpe"])
