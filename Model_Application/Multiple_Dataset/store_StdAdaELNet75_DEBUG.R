StdAdaELNet75.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/std_adaelnet5_DEBUG.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
StdAdaELNet75.error <- list()
StdAdaELNet75.result <- list()
StdAdaELNet75.final <- list()
#split data into separate error and result lists
for(i in 1:length(StdAdaELNet75.debug)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       StdAdaELNet75.error[[i]] <- list(error = StdAdaELNet75.debug[[i]]$error , 
                                   condition = as.data.frame(unlist(StdAdaELNet75.debug[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       StdAdaELNet75.result[[i]] <- StdAdaELNet75.debug[[i]]$result
#fill final list
       if(!is.null(StdAdaELNet75.debug[[i]]$result)) {
              StdAdaELNet75.final[[i]] <- StdAdaELNet75.debug[[i]]$result$important
       } else {
              StdAdaELNet75.final[[i]] <- StdAdaELNet75.error[[i]]
       }
}

#combine diagnostics
diagnostics <- data.frame(matrix(ncol = 4 , nrow = length(StdAdaELNet75.debug)))
colnames(diagnostics) <- c("data.seed" , "model.seed.ridge" , "model.seed.prenu" , "model.seed.nu")
for(i in 1:length(StdAdaELNet75.final)) {
        diagnostics[i , "data.seed"] <- StdAdaELNet75.final[[i]]$diagnostics$data.seed
        diagnostics[i , "model.seed.ridge"] <- StdAdaELNet75.final[[i]]$diagnostics$model.seed.ridge
        diagnostics[i , "model.seed.prenu"] <- StdAdaELNet75.final[[i]]$diagnostics$model.seed.prenu
        diagnostics[i , "model.seed.nu"] <- StdAdaELNet75.final[[i]]$diagnostics$model.seed.nu
}

#save files
saveRDS(StdAdaELNet75.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/StdAdaELNet75_result_DEBUG.RData")
saveRDS(StdAdaELNet75.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/StdAdaELNet75_error_DEBUG.RData")
saveRDS(StdAdaELNet75.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/StdAdaELNet75_resultmain_DEBUG.RData")
saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/StdAdaELNet75_diagnostics_DEBUG.RData")
