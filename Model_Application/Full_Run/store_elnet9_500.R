elnet9.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/elnet9_500.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
elnet9.error <- list()
elnet9.result <- list()
elnet9.final <- list()
#split data into separate error and result lists
for(i in 1:length(elnet9.half)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       elnet9.error[[i]] <- list(error = elnet9.half[[i]]$error , 
                                   condition = as.data.frame(unlist(elnet9.half[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       elnet9.result[[i]] <- elnet9.half[[i]]$result
#fill final list
       if(!is.null(elnet9.half[[i]]$result)) {
              elnet9.final[[i]] <- elnet9.half[[i]]$result$important
       } else {
              elnet9.final[[i]] <- elnet9.error[[i]]
       }
}

#combine diagnostics
#diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(elnet9.half)))
#colnames(diagnostics) <- c("data.seed" , "model.seed.elnet")
#for(i in 1:length(elnet9.final)) {
#        diagnostics[i , "data.seed"] <- elnet9.final[[i]]$diagnostics$data.seed
#        diagnostics[i , "model.seed.elnet"] <- elnet9.final[[i]]$diagnostics$model.seed.elnet
#}

#save files
saveRDS(elnet9.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/elnet9_result_500.RData")
saveRDS(elnet9.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/elnet9_error_500.RData")
saveRDS(elnet9.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/elnet9_resultmain_500.RData")
#saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/elnet9_diagnostics_500.RData")
