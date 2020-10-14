oshuberlasso.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/OSHuberLasso_500.RData")


#dealing with error/result from map(safely())
#create empty lists for error + result
oshuberlasso.error <- list()
oshuberlasso.result <- list()
oshuberlasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(oshuberlasso.half)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  oshuberlasso.error[[i]] <- list(error = oshuberlasso.half[[i]]$error , 
                              condition = as.data.frame(unlist(oshuberlasso.half[[i]]$condition) , 
                                                        n = n , p = p , 
                                                        eta.x = eta.x , eta.y = eta.y , 
                                                        g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  oshuberlasso.result[[i]] <- oshuberlasso.half[[i]]$result
  #fill final list
  if(!is.null(oshuberlasso.half[[i]]$result)) {
    oshuberlasso.final[[i]] <- oshuberlasso.half[[i]]$result$important
  } else {
    oshuberlasso.final[[i]] <- oshuberlasso.error[[i]]
  }
}

diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(oshuberlasso.half)))
colnames(diagnostics) <- c("data.seed" , "model.seed.lasso")
for(i in 1:length(oshuberlasso.final)) {
  cat("i = " , i , "\n")
  diagnostics[i , "data.seed"] <- oshuberlasso.final[[i]]$diagnostics$data.seed
  diagnostics[i , "model.seed.lasso"] <- oshuberlasso.final[[i]]$diagnostics$model.seed.lasso
}

#save files
saveRDS(oshuberlasso.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/oshuberlasso_result_500.RData")
saveRDS(oshuberlasso.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/oshuberlasso_error_500.RData")
saveRDS(oshuberlasso.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/oshuberlasso_resultmain_500.RData")
saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/oshuberlasso_diagnostics_500.RData")
