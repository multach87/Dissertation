msadaelnet5.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/msaelnet5_DEBUG.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
msadaelnet5.error <- list()
msadaelnet5.result <- list()
msadaelnet5.final <- list()
#split data into separate error and result lists
for(i in 1:length(msadaelnet5.debug)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       msadaelnet5.error[[i]] <- list(error = msadaelnet5.debug[[i]]$error , 
                                   condition = as.data.frame(unlist(msadaelnet5.debug[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       msadaelnet5.result[[i]] <- msadaelnet5.debug[[i]]$result
#fill final list
       if(!is.null(msadaelnet5.debug[[i]]$result)) {
              msadaelnet5.final[[i]] <- msadaelnet5.debug[[i]]$result$important
       } else {
              msadaelnet5.final[[i]] <- msadaelnet5.error[[i]]
       }
}

#combine diagnostics
#diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(msadaelnet5.debug)))
#colnames(diagnostics) <- c("data.seed" , "model.seed.elnet")
#for(i in 1:length(msadaelnet5.final)) {
#        diagnostics[i , "data.seed"] <- msadaelnet5.final[[i]]$diagnostics$data.seed
#        diagnostics[i , "model.seed.elnet"] <- msadaelnet5.final[[i]]$diagnostics$model.seed.elnet
#}

#save files
saveRDS(msadaelnet5.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/msadaelnet5_result_DEBUG.RData")
saveRDS(msadaelnet5.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/msadaelnet5_error_DEBUG.RData")
saveRDS(msadaelnet5.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msadaelnet5_resultmain_DEBUG.RData")
#saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/msadaelnet5_diagnostics_DEBUG.RData")
