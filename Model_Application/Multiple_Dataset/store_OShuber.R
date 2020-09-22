OSHuber.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/OSHuberLasso_debug.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
OSHuber.error <- list()
OSHuber.result <- list()
OSHuber.final <- list()
#split data into separate error and result lists
for(i in 1:length(OSHuber.debug)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       OSHuber.error[[i]] <- list(error = OSHuber.debug[[i]]$error , 
                                   condition = as.data.frame(unlist(OSHuber.debug[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       OSHuber.result[[i]] <- OSHuber.debug[[i]]$result
#fill final list
       if(!is.null(OSHuber.debug[[i]]$result)) {
              OSHuber.final[[i]] <- OSHuber.debug[[i]]$result$important
       } else {
              OSHuber.final[[i]] <- OSHuber.error[[i]]
       }
}

#combine diagnostics
diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(OSHuber.debug)))
colnames(diagnostics) <- c("data.seed" , "model.seed.lasso")
for(i in 1:length(OSHuber.final)) {
        diagnostics[i , "data.seed"] <- OSHuber.final[[i]]$diagnostics$data.seed
        diagnostics[i , "model.seed.lasso"] <- OSHuber.final[[i]]$diagnostics$model.seed.lasso
}

#save files
saveRDS(OSHuber.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/OSHuber_result_DEBUG.RData")
saveRDS(OSHuber.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/OSHuber_error_DEBUG.RData")
saveRDS(OSHuber.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/OSHuber_resultmain_DEBUG.RData")
saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/OSHuber_diagnostics_DEBUG.RData")
