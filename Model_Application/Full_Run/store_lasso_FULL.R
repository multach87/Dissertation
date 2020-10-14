lasso.full <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/lasso_full.RData")


#dealing with error/result from map(safely())
#create empty lists for error + result
lasso.error <- list()
lasso.result <- list()
lasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(lasso.full)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  lasso.error[[i]] <- list(error = lasso.full[[i]]$error , 
                              condition = as.data.frame(unlist(lasso.full[[i]]$condition) , 
                                                        n = n , p = p , 
                                                        eta.x = eta.x , eta.y = eta.y , 
                                                        g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  lasso.result[[i]] <- lasso.full[[i]]$result
  #fill final list
  if(!is.null(lasso.full[[i]]$result)) {
    lasso.final[[i]] <- lasso.full[[i]]$result$important
  } else {
    lasso.final[[i]] <- lasso.error[[i]]
  }
}

diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(lasso.full)))
colnames(diagnostics) <- c("data.seed" , "model.seed")
for(i in 1:length(lasso.final)) {
  diagnostics[i , "data.seed"] <- lasso.final[[i]]$diagnostics$data.seed
  diagnostics[i , "model.seed"] <- lasso.final[[i]]$diagnostics$model.seed
}

#save files
saveRDS(lasso.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/lasso_result_FULL.RData")
saveRDS(lasso.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/lasso_error_FULL.RData")
saveRDS(lasso.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/lasso_resultmain_FULL.RData")
saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/lasso_diagnostics_FULL.RData")
