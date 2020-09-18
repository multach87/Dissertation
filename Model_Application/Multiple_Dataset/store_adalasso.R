adalasso.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adalasso_debug.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
adalasso.error <- list()
adalasso.result <- list()
adalasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(adalasso.debug)) { 
  #iteration tracker
  cat("i = " , i , "\n")
  #fill error list
  adalasso.error[[i]] <- list(error = adalasso.debug[[i]]$error , 
                              condition = as.data.frame(unlist(adalasso.debug[[i]]$condition) , 
                                                        n = n , p = p , 
                                                        eta.x = eta.x , eta.y = eta.y , 
                                                        g = g , h = h , seed = seed))
  #fill in results if results aren't NULL from safely()
  adalasso.result[[i]] <- adalasso.debug[[i]]$result
  #fill final list
  if(!is.null(adalasso.debug[[i]]$result)) {
    adalasso.final[[i]] <- adalasso.debug[[i]]$result$important
  } else {
    adalasso.final[[i]] <- adalasso.error[[i]]
  }
}

#combine diagnostics
diagnostics <- data.frame(matrix(ncol = 4 , nrow = length(adalasso.debug)))
colnames(diagnostics) <- c("data.seed" , "model.seed.ridge" , "model.seed.prenu" , "model.seed.nu")
for(i in 1:length(adalasso.final)) {
  diagnostics[i , "data.seed"] <- adalasso.final[[i]]$diagnostics$data.seed
  diagnostics[i , "model.seed.ridge"] <- adalasso.final[[i]]$diagnostics$model.seed.ridge
  diagnostics[i , "model.seed.prenu"] <- adalasso.final[[i]]$diagnostics$model.seed.prenu
  diagnostics[i , "model.seed.nu"] <- adalasso.final[[i]]$diagnostics$model.seed.nu
}

#save files
saveRDS(adalasso.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/adalasso_result_DEBUG.RData")
saveRDS(adalasso.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adalasso_error_DEBUG.RData")
saveRDS(adalasso.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adalasso_resultmain_DEBUG.RData")
saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/adalasso_diagnostics_DEBUG.RData")
