ELNet75.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet75_debug.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
ELNet75.error <- list()
ELNet75.result <- list()
ELNet75.final <- list()
#split data into separate error and result lists
for(i in 1:length(ELNet75.debug)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       ELNet75.error[[i]] <- list(error = ELNet75.debug[[i]]$error , 
                                   condition = as.data.frame(unlist(ELNet75.debug[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       ELNet75.result[[i]] <- ELNet75.debug[[i]]$result
#fill final list
       if(!is.null(ELNet75.debug[[i]]$result)) {
              ELNet75.final[[i]] <- ELNet75.debug[[i]]$result$important
       } else {
              ELNet75.final[[i]] <- ELNet75.error[[i]]
       }
}

#combine diagnostics
diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(ELNet75.debug)))
colnames(diagnostics) <- c("data.seed" , "model.seed.elnet")
for(i in 1:length(ELNet75.final)) {
        diagnostics[i , "data.seed"] <- ELNet75.final[[i]]$diagnostics$data.seed
        diagnostics[i , "model.seed.lasso"] <- ELNet75.final[[i]]$diagnostics$model.seed.elnet
}

#save files
saveRDS(ELNet75.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/ELNet75_result_DEBUG.RData")
saveRDS(ELNet75.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/ELNet75_error_DEBUG.RData")
saveRDS(ELNet75.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/ELNet75_resultmain_DEBUG.RData")
saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/ELNet75_diagnostics_DEBUG.RData")
