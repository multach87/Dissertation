#load glmnet
library(glmnet)

#load data
#data.full <- readRDS()
single.data <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/single_data_091120.RData")
X <- single.data$X
Y <- single.data$Y
p <- single.data$conditions$p

#train lasso
set.seed(1)
#lambda.lasso.try = seq(0.01,0.6,length.out=100)
lambda.lasso.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
lasso.model <- cv.glmnet(X , Y , family = "gaussian" ,
                         lambda = lambda.lasso.try)
lambda.lasso.opt <- lasso.model$lambda.min
lasso.coefs <- predict(lasso.model , type = "coefficients" ,
        s = lambda.lasso.opt)[2 : (p + 1)]

#Metrics
which(lasso.model$lambda == lambda.lasso.opt)
lasso.err <- list(mpe = lasso.model$cvm[which(lasso.model$lambda == lambda.lasso.opt)] , 
               sd = lasso.model$cvsd[which(lasso.model$lambda == lambda.lasso.opt)])
lasso.err
