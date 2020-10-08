#OSLassoPLUS.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/OSLassoPLUS_debug.RData")
OSLassoPLUS.500 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/OSlassoPLUS_500.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
OSLassoPLUS.error <- list()
OSLassoPLUS.result <- list()
OSLassoPLUS.final <- list()
#split data into separate error and result lists
for(i in 1:length(OSLassoPLUS.500)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       OSLassoPLUS.error[[i]] <- list(error = OSLassoPLUS.500[[i]]$error , 
                                   condition = as.data.frame(unlist(OSLassoPLUS.500[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       OSLassoPLUS.result[[i]] <- OSLassoPLUS.500[[i]]$result
#fill final list
       if(!is.null(OSLassoPLUS.500[[i]]$result)) {
              OSLassoPLUS.final[[i]] <- OSLassoPLUS.500[[i]]$result$important
       } else {
              OSLassoPLUS.final[[i]] <- OSLassoPLUS.error[[i]]
       }
}

#combine diagnostics
diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(OSLassoPLUS.500)))
colnames(diagnostics) <- c("data.seed" , "model.seed.lasso")
for(i in 1:length(OSLassoPLUS.final)) {
        if(!is.null(OSLassoPLUS.final[[i]]$diagnostics$data.seed)) {
                diagnostics[i , "data.seed"] <- OSLassoPLUS.final[[i]]$diagnostics$data.seed
                diagnostics[i , "model.seed.lasso"] <- OSLassoPLUS.final[[i]]$diagnostics$model.seed.lasso
        } else {
                diagnostics[i , "data.seed"] <- NA
                diagnostics[i , "model.seed.lasso"] <- NA
        }
}

#save files
saveRDS(OSLassoPLUS.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/OS_result_500.RData")
saveRDS(OSLassoPLUS.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/OS_error_500.RData")
saveRDS(OSLassoPLUS.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/OS_resultmain_500.RData")
saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/OS_diagnostics_500.RData")
