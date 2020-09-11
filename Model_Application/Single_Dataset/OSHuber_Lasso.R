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
#OS-Huber Lasso + CV functions
OSH.lasso <- function(X,Y,lambda.lasso.try,k=1.5){
       n<-length(Y)
       Y.orgn<- Y
       model.for.cv<- cv.glmnet(X, Y, family="gaussian",lambda=lambda.lasso.try)
       #cat("OSH.lasso/cv.glmnet" , "\n")
       model.est <- H.lasso(X,Y,lambda.lasso.try,k)
       #cat("OSH.lasso/H.lasso, j =" , j , "\n")
       fit.lasso <- model.est$fit
       #cat("OSH.lasso/fit.lasso, j =" , j , "\n")
       res.lasso <- Y - fit.lasso
       #cat("OSH.lasso/res.lasso, j =" , j , "\n")
       sigma.est<- mad(Y-fit.lasso)
       #cat("OSH.lasso/sigma.est, j =" , j , "\n")
       beta.pre<- model.est$coefficient
       #cat("OSH.lasso/beta.pre, j =" , j , "\n")
       gamma.est<-rep(0,n)
       
       Y.old<- Y
       tol = 10
       n.iter <- 0
       outliers.init<- abs(scale(Y-model.est$residual))
       n.outlier<- length(which(as.vector(outliers.init)>2.5))
       lambda.gamma<- sigma.est*qnorm((2*n-n.outlier)/(2*n))
       #cat("OSH.lasso/lambda.gamma" , "\n")
       
       while(tol>1e-4 & n.iter<100)
       {
              nonzero<-which(abs(res.lasso)>=lambda.gamma)
              gamma.est[nonzero]<- res.lasso[nonzero]
              Y.new<- Y.old - gamma.est
              model.est<- H.lasso(X,Y.new,lambda.lasso.try,k)
              beta.post<- model.est$coefficient
              res.lasso<- model.est$residual
              tol<- sum((beta.pre-beta.post)^2)
              n.iter<- n.iter+1
              beta.pre<- beta.post
       }
       #cat("OSH.lasso/whileloop" , "\n")
       Y.fit<- cbind(rep(1,n),X)%*%beta.post
       #cat("OSH.lasso/Y.fit" , "\n")
       object<- list(coefficient=beta.post,fit=fit.lasso,iter = n.iter, n.outlier=n.outlier,
                     sigma.est = sigma.est,gamma.est = gamma.est, lambda = model.est$lambda)
       #cat("OSH.lasso/object" , "\n")
}
OSH.lasso.cv<-function(X,Y,lambda.lasso.try,k=1.5,lambda.gamma.try){
       
       n<-length(Y)
       Y.orgn<- Y
       model.est <- H.lasso(X,Y,lambda.lasso.try,k)
       #cat("OSH.cv.lasso/H.lasso" , "\n")
       lambda.lasso.opt<- model.est$lambda.lasso.opt
       fit.lasso <- model.est$fit
       res.lasso <- Y - fit.lasso
       sigma.est<- mad(Y-fit.lasso)
       gamma.est<-rep(0,n)
       #cat("OSH.cv.lasso/gamma.est" , "\n")
       
       n.fold<- 5
       n.cv <- n/n.fold
       CV.error2<-CV.error<-rep(NA,length(lambda.gamma.try))
       #cat("OSH.cv.lasso/CV.error2" , "\n")
       Y.pred.cv<-matrix(NA,nrow=length(Y),ncol=length(lambda.gamma.try))
       Y.new<- Y
       cat("OSH.cv.lasso/Y.new" , "\n")
       
       for (tt in 1:length(lambda.gamma.try))
       {
              #cat("OSH.cv.lasso/ForTTLoop/pregamma.est.cv" , "\n")
              gamma.est.cv<-rep(0,n-n.cv)
              cat("OSH.cv.lasso/ForTTLoop/gamma.est.cv" , "\n")
              for (jj in 1:n.fold)
              {
                     sample.out.index<- (1+n.cv*(jj-1)):(n.cv*(jj))
                     X.train<- X[-sample.out.index,]
                     #cat("X.Train = " , X.train , "\n")
                     Y.train<- Y[-sample.out.index]
                     #cat("Y.Train = " ,Y.train , "\n")
                     X.test<- X[sample.out.index,]
                     cat("X.Test" , "jj = " , jj , "\n")
                     
                     #model.train.temp<- H.lasso(X.train,Y.train,c(lambda.lasso.opt,lambda.lasso.opt),k=k)
                     model.train.temp<- H.lasso(X.train,Y.train,lambda.lasso.try = seq(0.01,0.6,length.out=100),k=k)
                     beta.pre<- model.train.temp$coef
                     tol<-100; n.iter <- 0
                     while(tol>1e-4 & n.iter<51) 
                     {
                            nonzero<-which(abs(model.train.temp$resid)>=sigma.est*lambda.gamma.try[tt])
                            gamma.est.cv[nonzero]<- model.train.temp$resid[nonzero]
                            Y.train.new <- Y.train - gamma.est.cv
                            #model.train.temp<- H.lasso(X.train,Y.train.new,c(lambda.lasso.opt,lambda.lasso.opt),k=k)
                            model.train.temp<- H.lasso(X.train,Y.train,lambda.lasso.try = seq(0.01,0.6,length.out=100),k=k)
                            beta.post <- model.train.temp$coefficient
                            tol<- sum((beta.pre-beta.post)^2)
                            n.iter<- n.iter+1
                            beta.pre<- beta.post
                     }
                     
                     Y.pred.cv[sample.out.index,tt] <-cbind(rep(1,n.cv),X.test)%*%beta.post
              }
              
              CV.error[tt]<- mean((Y.pred.cv[,tt]-Y.orgn)^2)
              CV.error2[tt]<- mean(abs(Y.pred.cv[,tt]-Y.orgn))
       }
       cat("OSH.cv.lasso/for_tt" , "\n")
       
       lambda.gamma.opt<- lambda.gamma.try[which.min(CV.error)]
       cat("OSH.cv.lasso/lambda.gamma.opt" , "\n")
       
       model.opt<- H.lasso(X,Y.orgn,c(lambda.lasso.opt,lambda.lasso.opt),k)
       cat("OSH.cv.lasso/model.opt" , "\n")
       beta.pre<- model.opt$coef
       cat("OSH.cv.lasso/beta.pre" , "\n")
       
       tol<-100; n.iter <- 0
       while(tol>1e-6 & n.iter<100) 
       {
              nonzero<-which(abs(model.opt$resid)>=sigma.est*lambda.gamma.opt)
              gamma.est[nonzero]<- model.opt$resid[nonzero]
              Y.new <- Y - gamma.est
              model.opt<- H.lasso(X,Y.orgn,c(lambda.lasso.opt,lambda.lasso.opt),k=k)
              beta.post <-  model.opt$coefficient
              tol<- mean((beta.pre-beta.post)^2)
              n.iter<- n.iter+1
              beta.pre<- beta.post
       }
       cat("OSH.cv.lasso/while_loop" , "\n")
       
       Y.fit<- cbind(rep(1,n),X)%*%beta.post
       
       object<- list(coefficient=beta.post,fit=fit.lasso,iter = n.iter, lambda.lasso.opt = lambda.lasso.opt,
                     CV.error=CV.error, n.outlier=length(which(gamma.est!=0)),sigma.est = sigma.est,gamma.est = gamma.est, lambda.gamma.opt= lambda.gamma.opt)
       
}

#load data
#data.full <- readRDS()
single.data <- data.full[[1]]
X <- single.data$X
Y <- single.data$Y

#run OS-Huber lasso
lambda.lasso.try <- seq(0.01 , 0.6 , length.out = 100)
lambda.gamma.try <- seq(1 , 4 , length.out = 50)
OSHuber.model <- OSH.lasso(X = X , Y = Y , lambda.lasso.try = lambda.lasso.try)
