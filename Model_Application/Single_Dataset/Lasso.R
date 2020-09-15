#load glmnet
library(glmnet)

#load data
#data.full <- readRDS()
single.data <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Dissertation_Git/Data_Generation/Data_Storage/single_data_091520.RData")
X <- single.data[[1]]$X
Y <- single.data[[1]]$Y
p <- single.data[[1]]$conditions$p

#train lasso
set.seed(1)
#lambda.lasso.try = seq(0.01,0.6,length.out=100)
lambda.lasso.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
lasso.model <- cv.glmnet(X , Y , family = "gaussian" ,
                         lambda = lambda.lasso.try)
lambda.lasso.opt <- lasso.model$lambda.min
lasso.coefs <- predict(lasso.model , type = "coefficients" ,
        s = lambda.lasso.opt)[2 : (p + 1)]
model <- list(full.model = lasso.model , lambda = lambda.lasso.opt , coefs = lasso.coefs)

#Metrics
which(lasso.model$lambda == lambda.lasso.opt)
lasso.err <- list(mpe = lasso.model$cvm[which(lasso.model$lambda == lambda.lasso.opt)] , 
               mpe.sd = lasso.model$cvsd[which(lasso.model$lambda == lambda.lasso.opt)])
lasso.err
metrics <- list(mpe = lasso.model$cvm[which(lasso.model$lambda == lambda.lasso.opt)] , 
                mpe.sd = lasso.model$cvsd[which(lasso.model$lambda == lambda.lasso.opt)] , 
                fpr = length(which(lasso.coefs[c(5:p)] != 0)) / length(lasso.coefs[c(5:p)]) , 
                fnr = length(which(lasso.coefs[c(1:4)] == 0)) / length(lasso.coefs[1:4]))
