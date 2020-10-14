adaelnet5.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/adaelnet5_500.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
adaelnet5.error <- list()
adaelnet5.result <- list()
adaelnet5.final <- list()
#split data into separate error and result lists
for(i in 1:length(adaelnet5.half)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       adaelnet5.error[[i]] <- list(error = adaelnet5.half[[i]]$error , 
                                   condition = as.data.frame(unlist(adaelnet5.half[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       adaelnet5.result[[i]] <- adaelnet5.half[[i]]$result
#fill final list
       if(!is.null(adaelnet5.half[[i]]$result)) {
              adaelnet5.final[[i]] <- adaelnet5.half[[i]]$result$important
       } else {
              adaelnet5.final[[i]] <- adaelnet5.error[[i]]
       }
}

#combine diagnostics
#diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(adaelnet5.half)))
#colnames(diagnostics) <- c("data.seed" , "model.seed.elnet")
#for(i in 1:length(adaelnet5.final)) {
#        diagnostics[i , "data.seed"] <- adaelnet5.final[[i]]$diagnostics$data.seed
#        diagnostics[i , "model.seed.elnet"] <- adaelnet5.final[[i]]$diagnostics$model.seed.elnet
#}

#save files
saveRDS(adaelnet5.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/adaelnet5_result_500.RData")
saveRDS(adaelnet5.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adaelnet5_error_500.RData")
saveRDS(adaelnet5.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet5_resultmain_500.RData")
#saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/adaelnet5_diagnostics_500.RData")
