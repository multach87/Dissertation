msadaelnet75.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/msaelnet5_DEBUG.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
msadaelnet75.error <- list()
msadaelnet75.result <- list()
msadaelnet75.final <- list()
#split data into separate error and result lists
for(i in 1:length(msadaelnet75.debug)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       msadaelnet75.error[[i]] <- list(error = msadaelnet75.debug[[i]]$error , 
                                   condition = as.data.frame(unlist(msadaelnet75.debug[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       msadaelnet75.result[[i]] <- msadaelnet75.debug[[i]]$result
#fill final list
       if(!is.null(msadaelnet75.debug[[i]]$result)) {
              msadaelnet75.final[[i]] <- msadaelnet75.debug[[i]]$result$important
       } else {
              msadaelnet75.final[[i]] <- msadaelnet75.error[[i]]
       }
}

#combine diagnostics
#diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(msadaelnet75.debug)))
#colnames(diagnostics) <- c("data.seed" , "model.seed.elnet")
#for(i in 1:length(msadaelnet75.final)) {
#        diagnostics[i , "data.seed"] <- msadaelnet75.final[[i]]$diagnostics$data.seed
#        diagnostics[i , "model.seed.elnet"] <- msadaelnet75.final[[i]]$diagnostics$model.seed.elnet
#}

#save files
saveRDS(msadaelnet75.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/msadaelnet75_result_DEBUG.RData")
saveRDS(msadaelnet75.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/msadaelnet75_error_DEBUG.RData")
saveRDS(msadaelnet75.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msadaelnet75_resultmain_DEBUG.RData")
#saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/msadaelnet75_diagnostics_DEBUG.RData")
