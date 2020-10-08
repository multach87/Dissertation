StdAdaELNet9.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/std_adaelnet5_DEBUG.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
StdAdaELNet9.error <- list()
StdAdaELNet9.result <- list()
StdAdaELNet9.final <- list()
#split data into separate error and result lists
for(i in 1:length(StdAdaELNet9.debug)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       StdAdaELNet9.error[[i]] <- list(error = StdAdaELNet9.debug[[i]]$error , 
                                   condition = as.data.frame(unlist(StdAdaELNet9.debug[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       StdAdaELNet9.result[[i]] <- StdAdaELNet9.debug[[i]]$result
#fill final list
       if(!is.null(StdAdaELNet9.debug[[i]]$result)) {
              StdAdaELNet9.final[[i]] <- StdAdaELNet9.debug[[i]]$result$important
       } else {
              StdAdaELNet9.final[[i]] <- StdAdaELNet9.error[[i]]
       }
}

#combine diagnostics
diagnostics <- data.frame(matrix(ncol = 4 , nrow = length(StdAdaELNet9.debug)))
colnames(diagnostics) <- c("data.seed" , "model.seed.ridge" , "model.seed.prenu" , "model.seed.nu")
for(i in 1:length(StdAdaELNet9.final)) {
        diagnostics[i , "data.seed"] <- StdAdaELNet9.final[[i]]$diagnostics$data.seed
        diagnostics[i , "model.seed.ridge"] <- StdAdaELNet9.final[[i]]$diagnostics$model.seed.ridge
        diagnostics[i , "model.seed.prenu"] <- StdAdaELNet9.final[[i]]$diagnostics$model.seed.prenu
        diagnostics[i , "model.seed.nu"] <- StdAdaELNet9.final[[i]]$diagnostics$model.seed.nu
}

#save files
saveRDS(StdAdaELNet9.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/StdAdaELNet9_result_DEBUG.RData")
saveRDS(StdAdaELNet9.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/StdAdaELNet9_error_DEBUG.RData")
saveRDS(StdAdaELNet9.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/StdAdaELNet9_resultmain_DEBUG.RData")
saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/StdAdaELNet9_diagnostics_DEBUG.RData")
