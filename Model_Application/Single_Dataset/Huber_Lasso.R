#libraries
library(glmnet)

#load data
#data.full <- readRDS()
#full.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/")
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")



#load data
single.data <- debug.data[[10]]
X <- single.data[["X"]]
Y <- single.data[["Y"]]

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
       beta.pre<- as.numeric(model.est$beta)
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
              beta.post <- as.numeric(model.est$beta)
              tol<- sum((beta.pre-beta.post)^2)
              n.iter<- n.iter+1
              beta.pre<- beta.post
       }
       
       sigma.est<- mean(Y.new- (X%*%beta.post)^2)
       Y.fit<- X%*%beta.post
       Y.res<- Y.new - Y.fit
       
       #store number of nonzero coefs
       st.lad <- sum(beta.post)                                          # number nonzero
       
       #generate MSE and sd(MSE) for model
       mse.lad <- sum((Y - Y.fit) ^ 2) / (n - st.lad - 1)
       sd.mse.lad <- sd((Y - Y.fit) ^ 2 / (n - st.lad - 1))
       
       #store lambda
       lambda.lasso.opt = model.est$lambda
       
       object<- list(coefficient = beta.post , 
                     fit = Y.fit , 
                     iter = n.iter , 
                     sigma.est = sigma.est , 
                     mpe = mse.lad , 
                     mpe.sd = sd.mse.lad ,
                     lambda.opt = lambda.lasso.opt)

}

#run Huber lasso
set.seed(501)
lambda.lasso.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
Huber.model <- H.lasso(X = X , Y = Y , lambda.lasso.try = lambda.lasso.try)
Huber.model
