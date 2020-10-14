adaelnet75.half <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/adaelnet75_500.RData")

#dealing with error/result from map(safely())
#create empty lists for error + result
adaelnet75.error <- list()
adaelnet75.result <- list()
adaelnet75.final <- list()
#split data into separate error and result lists
for(i in 1:length(adaelnet75.half)) { 
#iteration tracker
       cat("i = " , i , "\n")
#fill error list
       adaelnet75.error[[i]] <- list(error = adaelnet75.half[[i]]$error , 
                                   condition = as.data.frame(unlist(adaelnet75.half[[i]]$result$condition) , 
                                                             n = n , p = p , 
                                                             eta.x = eta.x , eta.y = eta.y , 
                                                             g = g , h = h , seed = seed))
#fill in results if results aren't NULL from safely()
       adaelnet75.result[[i]] <- adaelnet75.half[[i]]$result
#fill final list
       if(!is.null(adaelnet75.half[[i]]$result)) {
              adaelnet75.final[[i]] <- adaelnet75.half[[i]]$result$important
       } else {
              adaelnet75.final[[i]] <- adaelnet75.error[[i]]
       }
}

#combine diagnostics
#diagnostics <- data.frame(matrix(ncol = 2 , nrow = length(adaelnet75.half)))
#colnames(diagnostics) <- c("data.seed" , "model.seed.elnet")
#for(i in 1:length(adaelnet75.final)) {
#        diagnostics[i , "data.seed"] <- adaelnet75.final[[i]]$diagnostics$data.seed
#        diagnostics[i , "model.seed.elnet"] <- adaelnet75.final[[i]]$diagnostics$model.seed.elnet
#}

#save files
saveRDS(adaelnet75.result , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Model_Storage/adaelnet75_result_500.RData")
saveRDS(adaelnet75.error , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/adaelnet75_error_500.RData")
saveRDS(adaelnet75.final , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/adaelnet75_resultmain_500.RData")
#saveRDS(diagnostics , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Diagnostics_Storage/adaelnet75_diagnostics_500.RData")
