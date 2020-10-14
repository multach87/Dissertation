adalasso.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/adalasso_500.RData")


#dealing with error/result from map(safely())
#create empty lists for error + result
adalasso.error <- list()
adalasso.result <- list()
adalasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(adalasso.half)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  adalasso.error[[i]] <- list(error = adalasso.half[[i]]$error , 
                              condition = as.data.frame(unlist(adalasso.half[[i]]$condition) , 
                                                        n = n , p = p , 
                                                        eta.x = eta.x , eta.y = eta.y , 
                                                        g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  adalasso.result[[i]] <- adalasso.half[[i]]$result
  #fill final list
  if(!is.null(adalasso.half[[i]]$result)) {
    adalasso.final[[i]] <- adalasso.half[[i]]$result$important
  } else {
    adalasso.final[[i]] <- adalasso.error[[i]]
  }
}

diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(adalasso.half)))
colnames(diagnostics) <- c("data.seed" , "model.seed")
for(i in 1:length(adalasso.final)) {
  diagnostics[i , "data.seed"] <- adalasso.final[[i]]$diagnostics$data.seed
  diagnostics[i , "model.seed"] <- adalasso.final[[i]]$diagnostics$model.seed
}

#save files
saveRDS(adalasso.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/adalasso_result_500.RData")
saveRDS(adalasso.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adalasso_error_500.RData")
saveRDS(adalasso.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adalasso_resultmain_500.RData")
saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/adalasso_diagnostics_500.RData")
