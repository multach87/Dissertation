ladlasso.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/ladlasso_debug.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
ladlasso.error <- list()
ladlasso.result <- list()
ladlasso.final <- list()
#split data into separate error and result lists
for(i in 1:length(ladlasso.debug)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       ladlasso.error[[i]] <- list(error = ladlasso.debug[[i]]$error , 
                                   condition = as.data.frame(unlist(ladlasso.debug[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       ladlasso.result[[i]] <- ladlasso.debug[[i]]$result
#fill final list
       if(!is.null(ladlasso.debug[[i]]$result)) {
              ladlasso.final[[i]] <- ladlasso.debug[[i]]$result$important
       } else {
              ladlasso.final[[i]] <- ladlasso.error[[i]]
       }
}

#combine diagnostics
diagnostics <- data.frame(matrix(ncol = 4 , nrow = length(ladlasso.debug)))
colnames(diagnostics) <- c("data.seed" , "model.seed.ridge" , "model.seed.prenu" , "model.seed.nu")
for(i in 1:length(ladlasso.final)) {
        diagnostics[i , "data.seed"] <- ladlasso.final[[i]]$diagnostics$data.seed
        diagnostics[i , "model.seed.ridge"] <- ladlasso.final[[i]]$diagnostics$model.seed.ridge
        diagnostics[i , "model.seed.prenu"] <- ladlasso.final[[i]]$diagnostics$model.seed.prenu
        diagnostics[i , "model.seed.nu"] <- ladlasso.final[[i]]$diagnostics$model.seed.nu
}

#save files
saveRDS(ladlasso.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/ladlasso_result_DEBUG.RData")
saveRDS(ladlasso.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/ladlasso_error_DEBUG.RData")
saveRDS(ladlasso.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/ladlasso_resultmain_DEBUG.RData")
saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/ladlasso_diagnostics_DEBUG.RData")
