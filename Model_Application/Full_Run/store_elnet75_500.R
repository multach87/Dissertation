elnet75.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/elnet75_500.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
elnet75.error <- list()
elnet75.result <- list()
elnet75.final <- list()
#split data into separate error and result lists
for(i in 1:length(elnet75.half)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       elnet75.error[[i]] <- list(error = elnet75.half[[i]]$error , 
                                   condition = as.data.frame(unlist(elnet75.half[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       elnet75.result[[i]] <- elnet75.half[[i]]$result
#fill final list
       if(!is.null(elnet75.half[[i]]$result)) {
              elnet75.final[[i]] <- elnet75.half[[i]]$result$important
       } else {
              elnet75.final[[i]] <- elnet75.error[[i]]
       }
}

#combine diagnostics
#diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(elnet75.half)))
#colnames(diagnostics) <- c("data.seed" , "model.seed.elnet")
#for(i in 1:length(elnet75.final)) {
#        diagnostics[i , "data.seed"] <- elnet75.final[[i]]$diagnostics$data.seed
#        diagnostics[i , "model.seed.elnet"] <- elnet75.final[[i]]$diagnostics$model.seed.elnet
#}

#save files
saveRDS(elnet75.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/elnet75_result_500.RData")
saveRDS(elnet75.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet75_error_500.RData")
saveRDS(elnet75.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet75_resultmain_500.RData")
#saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/elnet75_diagnostics_500.RData")
