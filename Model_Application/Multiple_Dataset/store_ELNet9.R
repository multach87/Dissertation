ELNet9.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet9_debug.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
ELNet9.error <- list()
ELNet9.result <- list()
ELNet9.final <- list()
#split data into separate error and result lists
for(i in 1:length(ELNet9.debug)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       ELNet9.error[[i]] <- list(error = ELNet9.debug[[i]]$error , 
                                   condition = as.data.frame(unlist(ELNet9.debug[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       ELNet9.result[[i]] <- ELNet9.debug[[i]]$result
#fill final list
       if(!is.null(ELNet9.debug[[i]]$result)) {
              ELNet9.final[[i]] <- ELNet9.debug[[i]]$result$important
       } else {
              ELNet9.final[[i]] <- ELNet9.error[[i]]
       }
}

#combine diagnostics
diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(ELNet9.debug)))
colnames(diagnostics) <- c("data.seed" , "model.seed.elnet")
for(i in 1:length(ELNet9.final)) {
        diagnostics[i , "data.seed"] <- ELNet9.final[[i]]$diagnostics$data.seed
        diagnostics[i , "model.seed.lasso"] <- ELNet9.final[[i]]$diagnostics$model.seed.elnet
}

#save files
saveRDS(ELNet9.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/ELNet9_result_DEBUG.RData")
saveRDS(ELNet9.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/ELNet9_error_DEBUG.RData")
saveRDS(ELNet9.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/ELNet9_resultmain_DEBUG.RData")
saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/ELNet9_diagnostics_DEBUG.RData")
