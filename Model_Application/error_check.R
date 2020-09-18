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

