#load libraries
library(glmnet)

#load data
#data.full <- readRDS()
#full.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/")
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")


#OS Lasso function
OS.lasso<- function(X,Y,lambda.lasso.try,lambda.gamma.try){
       n<-length(Y)
       Y.orgn<- Y
       model.for.cv<- cv.glmnet(X, Y, family="gaussian",lambda=lambda.lasso.try)
       lambda.lasso.opt<- model.for.cv$lambda.min
       model.est<- glmnet(X,Y,family="gaussian",lambda=lambda.lasso.opt)
       fit.lasso<-  predict(model.est,X,s=lambda.lasso.opt)
       res.lasso<- Y - fit.lasso
       sigma.est<- mad(Y-fit.lasso)
       beta.est<- as.numeric(model.est$beta)
       gamma.est<-rep(0,n)
       
       n.fold<- 5
       n.cv <- n/n.fold
       CV.error2<-CV.error<-rep(NA,length(lambda.gamma.try))
       Y.pred.cv<-matrix(NA,nrow=length(Y),ncol=length(lambda.gamma.try))
       Y.new<- Y
       
       for (tt in 1:length(lambda.gamma.try))
       {
              gamma.est.cv<-rep(0,n-n.cv)
              
              for (jj in 1:n.fold)
              {
                     sample.out.index<- (1+n.cv*(jj-1)):(n.cv*(jj))
                     X.train<- X[-sample.out.index,]
                     Y.train<- Y[-sample.out.index]
                     X.test<- X[sample.out.index,]
                     
                     model.train.temp<- glmnet(X.train,Y.train,family="gaussian",lambda=lambda.lasso.opt)
                     beta.pre<-beta.post<- c(model.train.temp$a0,as.numeric(model.train.temp$beta))
                     tol<-100; n.iter <- 0
                     while(tol>1e-6 & n.iter<100) 
                     {
                            resid.temp<- Y.train-cbind(rep(1,n-n.cv),X.train)%*%beta.pre
                            nonzero<-which(abs(resid.temp)>=sigma.est*lambda.gamma.try[tt])
                            gamma.est.cv[nonzero]<- resid.temp[nonzero]
                            Y.train.new <- Y.train - gamma.est.cv
                            model.train.temp<- glmnet(X.train,Y.train.new,family="gaussian",lambda=lambda.lasso.opt)
                            beta.post <- c(model.train.temp$a0,as.numeric(model.train.temp$beta))
                            tol<- sum((beta.pre-beta.post)^2)
                            n.iter<- n.iter+1
                            beta.pre<-beta.post
                     }
                     
                     Y.pred.cv[sample.out.index,tt] <-cbind(rep(1,n.cv),X.test)%*%beta.post
              }
              
              CV.error[tt]<- mean((Y.pred.cv[,tt]-Y.orgn)^2)
              CV.error2[tt]<- mean(abs(Y.pred.cv[,tt]-Y.orgn))
              
       }
       
       lambda.gamma.opt<- lambda.gamma.try[which.min(CV.error)]
       
       model.opt<- glmnet(X,Y.orgn,family="gaussian",lambda=lambda.lasso.opt)
       beta.pre<- beta.post<- c(model.opt$a0,as.numeric(model.opt$beta))
       
       tol<-100; n.iter <- 0
       while(tol>1e-6 & n.iter<100) 
       {
              resid.opt<- Y.orgn-cbind(rep(1,n),X)%*%beta.pre
              nonzero<-which(abs(resid.opt)>=sigma.est*lambda.gamma.opt)
              gamma.est[nonzero]<- resid.opt[nonzero]
              Y.new <- Y.orgn - gamma.est
              model.opt<- glmnet(X,Y.new,family="gaussian",lambda=lambda.lasso.opt)
              beta.post <-  c(model.opt$a0,as.numeric(model.opt$beta))
              tol<- mean((beta.pre-beta.post)^2)
              n.iter<- n.iter+1
              beta.pre<-beta.post
       }
       
       Y.fit<- cbind(rep(1,n),X)%*%beta.post
       
       
       object<- list(coefficient=beta.post,fit=fit.lasso,iter = n.iter, sigma.est = sigma.est,CV.error=CV.error, n.outlier=length(which(gamma.est!=0)),
                     gamma.est = gamma.est, lambda.opt=lambda.gamma.opt)
}

#load data
#data.full <- readRDS()
single.data <- debug.data[[1]]
X <- single.data$X
Y <- single.data$Y

#run OS lasso
lambda.lasso.try <- seq(0.01 , 0.6 , length.out = 100)
lambda.gamma.try <- seq(1 , 4 , length.out = 50)
OS.model <- OS.lasso(X = X , Y = Y , lambda.lasso.try = lambda.lasso.try ,
                      lambda.gamma.try = lambda.gamma.try)
