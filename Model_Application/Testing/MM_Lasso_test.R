#libraries
library(pense)

#load data
single.data <- readRDS("XXX")
X <- single.data[["X"]]
Y <- single.data[["Y"]]

#run MM lasso
set.seed(501)
MM_model <- pensem(x = X , y = Y , alpha = 1.0 , 
                   nlambda = 20 , cv_k = 5)
MM_model$coefficients
saveRDS(MM_model$coefficients , "XXX")

