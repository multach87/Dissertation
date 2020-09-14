#libraries
library(pense)

#load data
single.data <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Dissertation_Git/Data_Generation/Data_Storage/single_data_091120.RData")
X <- single.data[["X"]]
Y <- single.data[["Y"]]

#run MM lasso
set.seed(501)
est_s <- pense(x = X , y = Y , alpha = 1.0 , 
               nlambda = 20 , cv_k = 5)
est_mm <- pensem(est_s , alpha = 1.0 , 
                 nlambda = 20 , cv_k = 5)
est_mm$coefficients
saveRDS(est_mm$coefficients , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Dissertation_Git/Model_Application/Testing/est_mm_coefs.RData")

set.seed(501)
MM_model <- pensem(x = X , y = Y , alpha = 1.0 , 
                   nlambda = 20 , cv_k = 5)
MM_model$coefficients
saveRDS(MM_model$coefficients , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Dissertation_Git/Model_Application/Testing/MM_model_coefs.RData")

