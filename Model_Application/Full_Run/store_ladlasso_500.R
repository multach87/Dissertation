ladlasso.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_500.RData")


#dealing with error/result from map(safely())
#create empty lists for error + result
ladlasso.error <- list()
ladlasso.result <- list()
ladlasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(ladlasso.half)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  ladlasso.error[[i]] <- list(error = ladlasso.half[[i]]$error , 
                              condition = as.data.frame(unlist(ladlasso.half[[i]]$condition) , 
                                                        n = n , p = p , 
                                                        eta.x = eta.x , eta.y = eta.y , 
                                                        g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  ladlasso.result[[i]] <- ladlasso.half[[i]]$result
  #fill final list
  if(!is.null(ladlasso.half[[i]]$result)) {
    ladlasso.final[[i]] <- ladlasso.half[[i]]$result$important
  } else {
    ladlasso.final[[i]] <- ladlasso.error[[i]]
  }
}

diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(ladlasso.half)))
colnames(diagnostics) <- c("data.seed" , "model.seed.ridge")
for(i in 1:length(ladlasso.final)) {
  cat("i = " , i , "\n")
  diagnostics[i , "data.seed"] <- ladlasso.final[[i]]$diagnostics$data.seed
  diagnostics[i , "model.seed.ridge"] <- ladlasso.final[[i]]$diagnostics$model.seed.ridge
}

#save files
saveRDS(ladlasso.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/ladlasso_result_500.RData")
saveRDS(ladlasso.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/ladlasso_error_500.RData")
saveRDS(ladlasso.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/ladlasso_resultmain_500.RData")
saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/ladlasso_diagnostics_500.RData")
