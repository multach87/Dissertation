Huber.full <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/Huberlasso_full.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
Huber.error <- list()
Huber.result <- list()
Huber.final <- list()
#split data into separate error and result lists
for(i in 1:length(Huber.full)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       Huber.error[[i]] <- list(error = Huber.full[[i]]$error , 
                                   condition = as.data.frame(unlist(Huber.full[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       Huber.result[[i]] <- Huber.full[[i]]$result
#fill final list
       if(!is.null(Huber.full[[i]]$result)) {
              Huber.final[[i]] <- Huber.full[[i]]$result$important
       } else {
              Huber.final[[i]] <- Huber.error[[i]]
       }
}

#combine diagnostics
diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(Huber.full)))
colnames(diagnostics) <- c("data.seed" , "model.seed.lasso")
for(i in 1:length(Huber.final)) {
        diagnostics[i , "data.seed"] <- Huber.final[[i]]$diagnostics$data.seed
        diagnostics[i , "model.seed.lasso"] <- Huber.final[[i]]$diagnostics$model.seed.lasso
}

#save files
saveRDS(Huber.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/Huber_result_FULL.RData")
saveRDS(Huber.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/Huber_error_FULL.RData")
saveRDS(Huber.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/Huber_resultmain_FULL.RData")
saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/Huber_diagnostics_FULL.RData")
