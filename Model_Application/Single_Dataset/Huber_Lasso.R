#libraries
library(glmnet)

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
H.lasso<- function(X,Y,lambda.lasso.try,k=1.5){
       n<-length(Y)
       Y.orgn<- Y
       model.for.cv<- cv.glmnet(X, Y, family="gaussian",lambda=lambda.lasso.try)
       lambda.lasso.opt<- model.for.cv$lambda.min 
       model.est<- glmnet(X,Y,family="gaussian",lambda=lambda.lasso.opt)
       fit.lasso<-  predict(model.est,X,s=lambda.lasso.opt)
       res.lasso<- Y-fit.lasso
       sigma.init<- mad(Y-fit.lasso)
       beta.pre<- c(model.est$a0,as.numeric(model.est$beta))
       Y.old<- Y
       tol = 10
       n.iter <- 0
       while(tol>1e-4 & n.iter<100)
       {
              Y.new<- fit.lasso + winsorized(res.lasso,a=k, sigma=sigma.init)
              model.for.cv<- cv.glmnet(X,Y.new, family="gaussian",lambda=lambda.lasso.try)
              model.est<- glmnet(X,Y.new,family="gaussian",lambda=model.for.cv$lambda.min )
              fit.lasso<-  predict(model.est,X,s=model.for.cv$lambda.min)
              res.lasso<- Y.new-fit.lasso
              beta.post <- c(model.est$a0,as.numeric(model.est$beta))
              tol<- sum((beta.pre-beta.post)^2)
              n.iter<- n.iter+1
              beta.pre<- beta.post
       }
       
       sigma.est<- mean((Y.new-cbind(rep(1,n),X)%*%beta.post)^2)
       Y.fit<- cbind(rep(1,n),X)%*%beta.post
       Y.res<- Y.new - Y.fit
       
       object<- list(coefficient=beta.post,fit=Y.fit, iter = n.iter, sigma.est = sigma.est,
                     lambda.lasso.opt = model.est$lambda, residual = Y.res)
}

#load data
#data.full <- readRDS()
single.data <- data.full[[1]]
X <- single.data$X
Y <- single.data$Y

#run Huber lasso
lambda.lasso.try <- seq(0.01 , 0.6 , length.out = 100)
Huber.model <- H.lasso(X = X , Y = Y , lambda.lasso.try = lambda.lasso.try)
