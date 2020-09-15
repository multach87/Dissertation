#load glmnet
library(glmnet)

#load data
#data.full <- readRDS()
debug.data <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/debug_data_091120.RData")
#select 10 random datasets from holdout data
set.seed(10)
testing.index <- c(sample(x = c(1:960) , size = 10 , replace = FALSE))
testing_10 <- debug.data[testing.index]

#What model information

#what metrics?



tracker <- as.vector(unlist(data$conditions)) 
#print tracker of status
cat("n = " , tracker[1] , "p = " , tracker[2] ,
    "eta.x = " , tracker[3] , "eta.y = " , tracker[4] , ";\n") 

#train lasso
set.seed(1)
lambda.lasso.try = seq(0.01,0.6,length.out=100)
lasso.model <- cv.glmnet(X , Y , family = "gaussian" ,
                         lambda = lambda.lasso.try)
lambda.lasso.opt <- lasso.model$lambda.min
lasso.coefs <- predict(lasso.model , type = "coefficients" ,
                       s = lambda.lasso.opt)[2 : (p + 1)]
