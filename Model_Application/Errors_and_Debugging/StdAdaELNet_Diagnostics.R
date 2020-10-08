#load testing10 simulation results
StdAdaELNet5.testing10 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/std_adaelnet5_TESTING10.RData")
StdAdaELNet75.testing10 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/std_adaelnet75_TESTING10.RData")
StdAdaELNet9.testing10 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/std_adaelnet9_TESTING10.RData")

#load testing10 data
testing10.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/testing10_data_091720.RData")

#extract seed columns from data
seeds.df <- data.frame(matrix(ncol = 50 , nrow = length(testing10.data)))
colnames(seeds.df) <- colnames(testing10.data[[1]]$seeds)
for(i in 1:length(testing10.data)) {
  seeds.df[i , ] <- testing10.data[[i]]$seeds
}

#fill .5 results
StdAdaELNet5.error <- list()
StdAdaELNet5.result <- list()
StdAdaELNet5.final <- list()
#split data into separate error and result lists
for(i in 1:length(StdAdaELNet5.testing10)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  StdAdaELNet5.error[[i]] <- list(error = StdAdaELNet5.testing10[[i]]$error , 
                                   condition = as.data.frame(unlist(StdAdaELNet5.testing10[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  StdAdaELNet5.result[[i]] <- StdAdaELNet5.testing10[[i]]$result
  #fill final list
  if(!is.null(StdAdaELNet5.testing10[[i]]$result)) {
    StdAdaELNet5.final[[i]] <- StdAdaELNet5.testing10[[i]]$result$important
  } else {
    StdAdaELNet5.final[[i]] <- StdAdaELNet5.error[[i]]
  }
}

#combine diagnostics.5
#diagnostics.5 <- data.frame(matrix(ncol = 4 , nrow = length(StdAdaELNet5.testing10)))
#colnames(diagnostics.5) <- c("data.seed" , "model.seed.ridge" , "model.seed.prenu" , "model.seed.nu")
#for(i in 1:length(StdAdaELNet5.final)) {
#  diagnostics.5[i , "data.seed"] <- StdAdaELNet5.final[[i]]$diagnostics$data.seed
#  diagnostics.5[i , "model.seed.ridge"] <- StdAdaELNet5.final[[i]]$diagnostics$model.seed.ridge
#  diagnostics.5[i , "model.seed.prenu"] <- StdAdaELNet5.final[[i]]$diagnostics$model.seed.prenu
#  diagnostics.5[i , "model.seed.nu"] <- StdAdaELNet5.final[[i]]$diagnostics$model.seed.nu
#}

#fill .75 results
StdAdaELNet75.error <- list()
StdAdaELNet75.result <- list()
StdAdaELNet75.final <- list()
#split data into separate error and result lists
for(i in 1:length(StdAdaELNet75.testing10)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  StdAdaELNet75.error[[i]] <- list(error = StdAdaELNet75.testing10[[i]]$error , 
                                   condition = as.data.frame(unlist(StdAdaELNet75.testing10[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  StdAdaELNet75.result[[i]] <- StdAdaELNet75.testing10[[i]]$result
  #fill final list
  if(!is.null(StdAdaELNet75.testing10[[i]]$result)) {
    StdAdaELNet75.final[[i]] <- StdAdaELNet75.testing10[[i]]$result$important
  } else {
    StdAdaELNet75.final[[i]] <- StdAdaELNet75.error[[i]]
  }
}

#combine diagnostics.75
#diagnostics.75 <- data.frame(matrix(ncol = 4 , nrow = length(StdAdaELNet75.testing10)))
#colnames(diagnostics.75) <- c("data.seed" , "model.seed.ridge" , "model.seed.prenu" , "model.seed.nu")
#for(i in 1:length(StdAdaELNet75.final)) {
#  diagnostics.75[i , "data.seed"] <- StdAdaELNet75.final[[i]]$diagnostics$data.seed
#  diagnostics.75[i , "model.seed.ridge"] <- StdAdaELNet75.final[[i]]$diagnostics$model.seed.ridge
#  diagnostics.75[i , "model.seed.prenu"] <- StdAdaELNet75.final[[i]]$diagnostics$model.seed.prenu
#  diagnostics.75[i , "model.seed.nu"] <- StdAdaELNet75.final[[i]]$diagnostics$model.seed.nu
#}

#fill .9 results
StdAdaELNet9.error <- list()
StdAdaELNet9.result <- list()
StdAdaELNet9.final <- list()
#split data into separate error and result lists
for(i in 1:length(StdAdaELNet9.testing10)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  StdAdaELNet9.error[[i]] <- list(error = StdAdaELNet9.testing10[[i]]$error , 
                                   condition = as.data.frame(unlist(StdAdaELNet9.testing10[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  StdAdaELNet9.result[[i]] <- StdAdaELNet9.testing10[[i]]$result
  #fill final list
  if(!is.null(StdAdaELNet9.testing10[[i]]$result)) {
    StdAdaELNet9.final[[i]] <- StdAdaELNet9.testing10[[i]]$result$important
  } else {
    StdAdaELNet9.final[[i]] <- StdAdaELNet9.error[[i]]
  }
}

#combine diagnostics.9
#diagnostics.9 <- data.frame(matrix(ncol = 4 , nrow = length(StdAdaELNet9.testing10)))
#colnames(diagnostics.9) <- c("data.seed" , "model.seed.ridge" , "model.seed.prenu" , "model.seed.nu")
#for(i in 1:length(StdAdaELNet9.final)) {
#  diagnostics.9[i , "data.seed"] <- StdAdaELNet9.final[[i]]$diagnostics$data.seed
#  diagnostics.9[i , "model.seed.ridge"] <- StdAdaELNet9.final[[i]]$diagnostics$model.seed.ridge
#  diagnostics.9[i , "model.seed.prenu"] <- StdAdaELNet9.final[[i]]$diagnostics$model.seed.prenu
#  diagnostics.9[i , "model.seed.nu"] <- StdAdaELNet9.final[[i]]$diagnostics$model.seed.nu
#}

#store results into dataframe to compare
#0.5
StdAdaELNet5.results.df <- data.frame(matrix(ncol = ncol(StdAdaELNet5.final[[1]]$info)))
colnames(StdAdaELNet5.results.df) <- colnames(StdAdaELNet5.final[[1]]$info)

#fill results
##ERRORS AT: NONE
for(i in 1:length(StdAdaELNet5.final)) {
  if(is.null(StdAdaELNet5.final[[i]]$error)) {
    StdAdaELNet5.results.df[i , ] <- StdAdaELNet5.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    StdAdaELNet5.results.df[i , 1:7] <- debug.data[[i]]$conditions
  }
}

mean(StdAdaELNet5.results.df[ , "fpr"] , na.rm = T)
mean(StdAdaELNet5.results.df[ , "fnr"] , na.rm = T)
mean(StdAdaELNet5.results.df[!is.infinite(StdAdaELNet5.results.df[ , "mpe"]) , "mpe"] , na.rm = T)



#0.75
StdAdaELNet75.results.df <- data.frame(matrix(ncol = ncol(StdAdaELNet75.final[[1]]$info)))
colnames(StdAdaELNet75.results.df) <- colnames(StdAdaELNet75.final[[1]]$info)

#fill results
##ERRORS AT: NONE
for(i in 1:length(StdAdaELNet75.final)) {
  if(is.null(StdAdaELNet75.final[[i]]$error)) {
    StdAdaELNet75.results.df[i , ] <- StdAdaELNet75.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    StdAdaELNet75.results.df[i , 1:7] <- debug.data[[i]]$conditions
  }
}


#0.9
StdAdaELNet9.results.df <- data.frame(matrix(ncol = ncol(StdAdaELNet9.final[[1]]$info)))
colnames(StdAdaELNet9.results.df) <- colnames(StdAdaELNet9.final[[1]]$info)

#fill results
##ERRORS AT: NONE
for(i in 1:length(StdAdaELNet9.final)) {
  if(is.null(StdAdaELNet9.final[[i]]$error)) {
    StdAdaELNet9.results.df[i , ] <- StdAdaELNet9.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    StdAdaELNet9.results.df[i , 1:7] <- debug.data[[i]]$conditions
  }
}



mean(StdAdaELNet5.results.df[ , "fpr"] , na.rm = T)
mean(StdAdaELNet5.results.df[ , "fnr"] , na.rm = T)
mean(StdAdaELNet5.results.df[!is.infinite(StdAdaELNet5.results.df[ , "mpe"]) , "mpe"] , na.rm = T)

mean(StdAdaELNet75.results.df[ , "fpr"] , na.rm = T)
mean(StdAdaELNet75.results.df[ , "fnr"] , na.rm = T)
mean(StdAdaELNet75.results.df[!is.infinite(StdAdaELNet75.results.df[ , "mpe"]) , "mpe"] , na.rm = T)

mean(StdAdaELNet9.results.df[ , "fpr"] , na.rm = T)
mean(StdAdaELNet9.results.df[ , "fnr"] , na.rm = T)
mean(StdAdaELNet9.results.df[!is.infinite(StdAdaELNet9.results.df[ , "mpe"]) , "mpe"] , na.rm = T)

