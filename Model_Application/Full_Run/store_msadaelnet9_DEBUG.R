msadaelnet9.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/msaelnet9_DEBUG.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
msadaelnet9.error <- list()
msadaelnet9.result <- list()
msadaelnet9.final <- list()
#split data into separate error and result lists
for(i in 1:length(msadaelnet9.debug)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       msadaelnet9.error[[i]] <- list(error = msadaelnet9.debug[[i]]$error , 
                                   condition = as.data.frame(unlist(msadaelnet9.debug[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       msadaelnet9.result[[i]] <- msadaelnet9.debug[[i]]$result
#fill final list
       if(!is.null(msadaelnet9.debug[[i]]$result)) {
              msadaelnet9.final[[i]] <- msadaelnet9.debug[[i]]$result$important
       } else {
              msadaelnet9.final[[i]] <- msadaelnet9.error[[i]]
       }
}

#combine diagnostics
#diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(msadaelnet9.debug)))
#colnames(diagnostics) <- c("data.seed" , "model.seed.elnet")
#for(i in 1:length(msadaelnet9.final)) {
#        diagnostics[i , "data.seed"] <- msadaelnet9.final[[i]]$diagnostics$data.seed
#        diagnostics[i , "model.seed.elnet"] <- msadaelnet9.final[[i]]$diagnostics$model.seed.elnet
#}

#save files
saveRDS(msadaelnet9.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/msadaelnet9_result_DEBUG.RData")
saveRDS(msadaelnet9.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/msadaelnet9_error_DEBUG.RData")
saveRDS(msadaelnet9.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/msadaelnet9_resultmain_DEBUG.RData")
#saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/msadaelnet9_diagnostics_DEBUG.RData")
