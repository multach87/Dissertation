#load libraries
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.full <- readRDS()
ribo <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/ribo_split.RData")

#winsorized function
winsorized<- function(x,a=1.5,sigma=1) {
  s<-sigma
  newx<-x
  indp<-x>(a*s)
  newx[indp]<-(a*s)
  indn<- x<(a*-s)
  newx[indn]<- (-a*s)
  return(newx)}
#Huber lasso function
HforOSH <- function(data){
  #load X, Y, n, p
  X <- data$X
  Y <- data$Y
  n <- length(Y)
  p <- data$conditions$p
  
  #set seed for generating initial lasso coefficients for weighting
  seed.lasso <- data$seeds[ , "seed.9"]
  set.seed(seed.lasso)
  
  #set possible lambda and gamma values
  lambda.lasso.try <- seq(log(0.01) , log(1400) , length.out = 100)
  lambda.lasso.try <- exp(lambda.lasso.try)
  
  #initial lasso model
  model.for.cv<- cv.glmnet(X, Y, family="gaussian",lambda=lambda.lasso.try)
  lambda.lasso.opt<- model.for.cv$lambda.min 
  model.est<- glmnet(X,Y,family="gaussian",lambda=lambda.lasso.opt)
  fit.lasso<-  predict(model.est,X,s=lambda.lasso.opt)
  res.lasso<- Y-fit.lasso
  sigma.init<- mad(Y-fit.lasso)
  beta.pre<- as.numeric(model.est$beta)
  Y.old<- Y
  tol = 10
  n.iter <- 0
  while(tol>1e-4 & n.iter<100)
  {
    Y.new<- fit.lasso + winsorized(res.lasso,a=1.5, sigma=sigma.init)
    model.for.cv<- cv.glmnet(X,Y.new, family="gaussian",lambda=lambda.lasso.try)
    model.est<- glmnet(X,Y.new,family="gaussian",lambda=model.for.cv$lambda.min )
    fit.lasso<-  predict(model.est,X,s=model.for.cv$lambda.min)
    res.lasso<- Y.new-fit.lasso
    beta.post <- as.numeric(model.est$beta)
    tol<- sum((beta.pre-beta.post)^2)
    n.iter<- n.iter+1
    beta.pre<- beta.post
  }
  
  sigma.est<- mean(Y.new-(X%*%beta.post)^2)
  Y.fit<- X%*%beta.post
  Y.res<- Y.new - Y.fit
  
  object<- list(coefficient=beta.post,fit=Y.fit, iter = n.iter, sigma.est = sigma.est,
                lambda.lasso.opt = model.est$lambda, residual = Y.res , seed = seed.lasso)
}
HforOSH2 <- function(X,Y,lambda.lasso.try){
  #load n
  n <- length(Y)
  
  #set seed for generating initial lasso coefficients for weighting
  #seed.lasso <- data$seeds[ , "seed.10"]
  #set.seed(seed.lasso)
  
  #set possible lambda and gamma values
  #lambda.lasso.try <- seq(log(0.01) , log(1400) , length.out = 100)
  #lambda.lasso.try <- exp(lambda.lasso.try)
  
  #initial lasso model
  model.for.cv<- cv.glmnet(X, Y, family="gaussian",lambda=lambda.lasso.try)
  lambda.lasso.opt<- model.for.cv$lambda.min 
  model.est<- glmnet(X,Y,family="gaussian",lambda=lambda.lasso.opt)
  fit.lasso<-  predict(model.est,X,s=lambda.lasso.opt)
  res.lasso<- Y-fit.lasso
  sigma.init<- mad(Y-fit.lasso)
  beta.pre<- as.numeric(model.est$beta)
  Y.old<- Y
  tol = 10
  n.iter <- 0
  while(tol>1e-4 & n.iter<100)
  {
    Y.new<- fit.lasso + winsorized(res.lasso,a=1.5, sigma=sigma.init)
    model.for.cv<- cv.glmnet(X,Y.new, family="gaussian",lambda=lambda.lasso.try)
    model.est<- glmnet(X,Y.new,family="gaussian",lambda=model.for.cv$lambda.min )
    fit.lasso<-  predict(model.est,X,s=model.for.cv$lambda.min)
    res.lasso<- Y.new-fit.lasso
    beta.post <- as.numeric(model.est$beta)
    tol<- sum((beta.pre-beta.post)^2)
    n.iter<- n.iter+1
    beta.pre<- beta.post
  }
  
  sigma.est<- mean(Y.new-(X%*%beta.post)^2)
  Y.fit<- X%*%beta.post
  Y.res<- Y.new - Y.fit
  
  object<- list(coefficient=beta.post,fit=Y.fit, iter = n.iter, sigma.est = sigma.est,
                lambda.lasso.opt = model.est$lambda, residual = Y.res) #, seed = seed.lasso
}


#oshuberlasso application function
oshuberlasso.sim.fnct <- function(data) {
        #create simulation tracker
        #tracker <- as.vector(unlist(data$conditions)) 
        
        #print tracker of status
        cat("iteration = " , data$track , ";\n")
      
       #load X, Y, p, n
       X <- as.matrix(data$train[ , -1])
       #cat("X = " , X , "\n")
       #cat("class(X) = " , class(X) , "\n")
       Y <- data$train[ , 1]
       Y.orgn<- Y
       #cat("class(Y) = " , class(Y) , "\n")
       n <- length(Y)
       #set possible lambda and gamma values
       lambda.lasso.try <- seq(log(0.01) , log(1400) , length.out = 100)
       lambda.lasso.try <- exp(lambda.lasso.try)
       lambda.gamma.try <- exp(seq(log(1) , log(1400) , length.out = 100))
       
       #initial lasso model
       model.for.cv<- cv.glmnet(X, Y, family="gaussian",lambda=lambda.lasso.try)
       lambda.lasso.opt<- model.for.cv$lambda.min 
       #cat("hello \n")
       
       model.est <- HforOSH2(X , Y , lambda.lasso.try)
       #cat("world \n")
       
       fit.lasso <- model.est$fit
       res.lasso<- Y - fit.lasso
       sigma.est<- mad(Y-fit.lasso)
       beta.pre<- as.numeric(model.est$coefficient)
       Y.old<- Y
       tol = 10
       n.iter <- 0
       #cat("hello2 \n")
       gamma.est<-rep(0,n)
       #cat("world2 \n")
       
       Y.old<- Y
       tol = 10
       n.iter <- 0
       outliers.init<- abs(scale(Y-model.est$residual))
       n.outlier<- length(which(as.vector(outliers.init)>2.5))
       lambda.gamma<- sigma.est*qnorm((2*n-n.outlier)/(2*n))
       
       while(tol>1e-4 & n.iter<100)
       {
         nonzero<-which(abs(res.lasso)>=lambda.gamma)
         gamma.est[nonzero]<- res.lasso[nonzero]
         Y.new<- Y.old - gamma.est
         model.est<- HforOSH2(X,Y.new,lambda.lasso.try)
         beta.post<- model.est$coefficient
         res.lasso<- model.est$residual
         tol<- sum((beta.pre-beta.post)^2)
         n.iter<- n.iter+1
         beta.pre<- beta.post
       }
       
       sigma.est<- mean(Y.new- (X%*%beta.post)^2)
       n.coefs <- sum(beta.post != 0)
       
       #specify test data
       test.X <- as.matrix(data$test[ , -1])
       #cat("class(test.X) = " , class(test.X) , "\n")
       test.Y <- data$test[ , 1]
       
       #apply to test set
       pred.y <- test.X %*% beta.post
       #cat("pred.y = " , pred.y , "\n")
       resid <- pred.y - test.Y
       resid.sq <- resid^2
       sum.resid.sq <- sum(resid.sq)
       mse <- sum.resid.sq / n

       #initialize important info dataframe
       #put conditions, model info, and metrics into list
       return(list(model = list(full.model = model.est , 
                                iter = n.iter , 
                                sigma.est = sigma.est , 
                                n.outlier = length(which(gamma.est != 0)) ,
                                gamma.est = gamma.est , 
                                lambda.lasso.opt = lambda.lasso.opt ,
                                #lambda.gamma.opt = lambda.gamma.opt , 
                                coefs = beta.post) , 
                   metrics = list(mse = mse , 
                                   n.coefs = n.coefs
                                  )
                   )
       )

}




#run across full dataset
oshuberlasso.full <- ribo %>%   
       map(safely(oshuberlasso.sim.fnct))

saveRDS(oshuberlasso.full , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Applied_Storage/oshuberlasso_ribo.RData")


