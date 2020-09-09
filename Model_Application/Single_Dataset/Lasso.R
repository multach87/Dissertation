#load glmnet
library(glmnet)

#load data
#data.full <- readRDS()
single.data <- data.full[[1]]
X <- single.data$X
Y <- single.data$Y
p <- single.data$conditions$p

#train lasso
set.seed(1)
lambda.lasso.try = seq(0.01,0.6,length.out=100)
lasso.model <- cv.glmnet(X , Y , family = "gaussian" ,
                         lambda = lambda.lasso.try)
lambda.lasso.opt <- lasso.model$lambda.min
lasso.coefs <- predict(lasso.model , type = "coefficients" ,
        s = lambda.lasso.opt)[2 : (p + 1)]
