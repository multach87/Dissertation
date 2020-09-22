#adaptive lasso: NULL
for(i in 1:length(adalasso.error)) {
  if(!is.null(adalasso.error[[i]]$error)) {
    cat("error at i = " , i , "\n")
  }
}

#LAD Lasso: errors at c(102 , 111 , 112 , 132 , 136 , 139 , 168 , 172 , 228 , 293 , 325 , 762)
for(i in 1:length(ladlasso.error)) {
  if(!is.null(ladlasso.error[[i]]$error)) {
    cat("error at i = " , i , "\n")
  }
}

#Standard lasso: NULL
for(i in 1:length(lasso.error)) {
  if(!is.null(lasso.error[[i]]$error)) {
    cat("error at i = " , i , "\n")
  }
}


#OSLassoPLUS: errors at c(92, 96 , 159)
OS.error <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/OS_error_DEBUG.RData")
for(i in 1:length(OS.error)) {
  if(!is.null(OS.error[[i]]$error)) {
    cat("error at i = " , i , "\n")
  }
}

#Huber lasso: NULL
Huber.error <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/Huber_error_debug.RData")
for(i in 1:length(Huber.error)) {
  if(!is.null(Huber.error[[i]]$error)) {
    cat("error at i = " , i , "\n")
  }
}

