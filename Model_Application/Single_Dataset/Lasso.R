#load glmnet
library(glmnet)

#load data
#data.full <- readRDS()
single.data <- data.full[[1]]

#train lasso
set.seed(1)
lambda.lasso.try = seq(0.01,0.6,length.out=100)
train <- sample(1:nrow(single.data$X) , ceiling(nrow(single.data$X) / 2))
pred_train <- single.data$X[train , ]
pred_test <- single.data$X[-train , ]
y_train <- single.data$Y[train]
y_test <- single.data$Y[-train]
lam <- cv.glmnet(pred_train , y_train , alpha = 1 , 
                 lambda = lambda.lasso.try , 
                 nfolds = floor(nrow(pred_train) / 3))$lambda.min
lasso_best <- glmnet()
                 