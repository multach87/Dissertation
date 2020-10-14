ELNet5.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet5_debug.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
ELNet5.error <- list()
ELNet5.result <- list()
ELNet5.final <- list()
#split data into separate error and result lists
for(i in 1:length(ELNet5.debug)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       ELNet5.error[[i]] <- list(error = ELNet5.debug[[i]]$error , 
                                   condition = as.data.frame(unlist(ELNet5.debug[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       ELNet5.result[[i]] <- ELNet5.debug[[i]]$result
#fill final list
       if(!is.null(ELNet5.debug[[i]]$result)) {
              ELNet5.final[[i]] <- ELNet5.debug[[i]]$result$important
       } else {
              ELNet5.final[[i]] <- ELNet5.error[[i]]
       }
}

#combine diagnostics
diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(ELNet5.debug)))
colnames(diagnostics) <- c("data.seed" , "model.seed.elnet")
for(i in 1:length(ELNet5.final)) {
        diagnostics[i , "data.seed"] <- ELNet5.final[[i]]$diagnostics$data.seed
        diagnostics[i , "model.seed.lasso"] <- ELNet5.final[[i]]$diagnostics$model.seed.elnet
}

#save files
saveRDS(ELNet5.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/ELNet5_result_DEBUG.RData")
saveRDS(ELNet5.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/ELNet5_error_DEBUG.RData")
saveRDS(ELNet5.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/ELNet5_resultmain_DEBUG.RData")
saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/ELNet5_diagnostics_DEBUG.RData")
