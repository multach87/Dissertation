#libraries
library(glmnet)
library(purrr)
library(magrittr)
library(pense)

#load data
#data.full <- readRDS()
#full.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/")
#debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
testing10.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/testing10_data_091720.RData")
single.data <- testing10.data[[1]]

#load X and Y
X <- single.data[["X"]]
Y <- single.data[["Y"]]

#test model
set.seed(501)
lambda.lasso.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
elnet.test <- pense(x = X , y = Y , alpha = 0.5 , lambda = lambda.lasso.try)
#summary(elnet.test) NOT USEFUL STUFF
elnet.test
#find minimizing results/use cv
elnet.cv <- pense_cv(x = X , y = Y , alpha = 0.5 , 
                     cv_k = 5 , cv_repl = 10 , 
                     lambda = lambda.lasso.try)
coef(elnet.cv)

#test adaptive elnet model
set.seed(501)
lambda.lasso.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
adaelnet.test <- adapense_cv(x = X , y = Y , alpha = 0.5 , 
                    cv_k = 5 , cv_repl = 10 , 
                    lambda = lambda.lasso.try)
summary(adaelnet.test)
