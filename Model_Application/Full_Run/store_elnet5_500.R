elnet5.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/elnet5_500.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
elnet5.error <- list()
elnet5.result <- list()
elnet5.final <- list()
#split data into separate error and result lists
for(i in 1:length(elnet5.half)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       elnet5.error[[i]] <- list(error = elnet5.half[[i]]$error , 
                                   condition = as.data.frame(unlist(elnet5.half[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       elnet5.result[[i]] <- elnet5.half[[i]]$result
#fill final list
       if(!is.null(elnet5.half[[i]]$result)) {
              elnet5.final[[i]] <- elnet5.half[[i]]$result$important
       } else {
              elnet5.final[[i]] <- elnet5.error[[i]]
       }
}

#combine diagnostics
#diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(elnet5.half)))
#colnames(diagnostics) <- c("data.seed" , "model.seed.elnet")
#for(i in 1:length(elnet5.final)) {
#        diagnostics[i , "data.seed"] <- elnet5.final[[i]]$diagnostics$data.seed
#        diagnostics[i , "model.seed.elnet"] <- elnet5.final[[i]]$diagnostics$model.seed.elnet
#}

#save files
saveRDS(elnet5.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/elnet5_result_500.RData")
saveRDS(elnet5.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet5_error_500.RData")
saveRDS(elnet5.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet5_resultmain_500.RData")
#saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/elnet5_diagnostics_500.RData")
