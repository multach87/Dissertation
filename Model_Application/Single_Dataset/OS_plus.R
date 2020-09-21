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

#KFold subsetter function
kfold_subsetter <- function(data , k , seed = 7 , list = FALSE , random = TRUE) {
       
       if(length(dim(data)) == 2) { ###For 2D data
              #determine number of larger subsets (when unequal subsets)
              nsams.large <- nrow(data) %% k
              
              #determine number of smaller subsets (total number when equal subsets)
              nsams.small <- k - nsams.large
              
              #determine sample size of larger subsets (when unequal subsets)
              samsize.large <- ceiling(nrow(data) / k) * (nsams.large != 0)
              
              #determine sample size of smaller subsets (all subset size when equal subsets)
              samsize.small <- floor(nrow(data) / k)
              
              #indicator for which subset
              subset.indicator <- c(rep((1 : k) , floor(nrow(data) / k)) ,
                                    rep((1 : (nsams.large) ) , (1 * (nsams.large != 0)) ))
              
              #fix random assignment process
              if(seed) {
                     set.seed(seed)
              }
              
              #combine subset indicator with original data  
              if(random) {
                      newdata <- cbind(data , subset = sample(subset.indicator))
              } else {
                      newdata <- cbind(data , subset = sort(subset.indicator))
              }
              
              if(list) {
                     newdata <- return(split(newdata[ , -ncol(newdata)] ,
                                             f = newdata[ , ncol(newdata)]))
              } else {
                     newdata <- return(newdata)
              }
       } else if (length(dim(data)) == 0){   #for 1D data
              #determine number of larger subsets (when unequal subsets)
              nsams.large <- length(data) %% k
              
              #determine number of smaller subsets (total number when equal subsets)
              nsams.small <- k - nsams.large
              
              #determine sample size of larger subsets (when unequal subsets)
              samsize.large <- ceiling(length(data) / k) * (nsams.large != 0)
              
              #determine sample size of smaller subsets (all subset size when equal subsets)
              samsize.small <- floor(length(data) / k)
              
              #indicator for which subset
              subset.indicator <- c(rep((1 : k) , floor(length(data) / k)) ,
                                    rep((1 : (nsams.large) ) , (1 * (nsams.large != 0)) ))
              
              #fix random assignment process
              if(seed) {
                     set.seed(seed)
              }
              
              #combine subset indicator with original data
              #create split list if desired
              newdata <- matrix(cbind(data , 
                                      subset = sample(subset.indicator)) , 
                                ncol = 2)
              if(list) {
                     newdata <- return(split(newdata[ , -ncol(newdata)] ,
                                             f = newdata[ , ncol(newdata)]))
              } else {
                     newdata <- return(newdata)
              }
       }
}
{
#with custom function
n<-length(Y)
n
Y.orgn<- Y
Y.orgn
lambda.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
set.seed(501)
model.for.cv<- cv.glmnet(X, Y, family="gaussian",lambda=lambda.try)
lambda.lasso.opt<- model.for.cv$lambda.min
lambda.lasso.opt
model.est<- glmnet(X,Y,family="gaussian",lambda=lambda.lasso.opt)
fit.lasso<-  predict(model.est,X,s=lambda.lasso.opt)
fit.lasso
res.lasso<- Y - fit.lasso
sigma.est<- mad(Y-fit.lasso)
beta.est<- as.numeric(model.est$beta)
gamma.est<-rep(0,n)

K <- 5
X.new <- kfold_subsetter(X , k = K)
Y.new <- cbind(Y , X.new[ , "subset"])
n.cv <- n/ K
CV.error2<-CV.error<-rep(NA,length(lambda.gamma.try))
Y.pred.cv<-matrix(NA,nrow=length(Y),ncol=length(lambda.gamma.try))


#without custom function
n<-length(Y)
n
Y.orgn<- Y
Y.orgn
set.seed(501)
model.for.cv<- cv.glmnet(X, Y, family="gaussian",lambda=lambda.try)
lambda.lasso.opt<- model.for.cv$lambda.min
lambda.lasso.opt
model.est<- glmnet(X,Y,family="gaussian",lambda=lambda.lasso.opt)
fit.lasso<-  predict(model.est,X,s=lambda.lasso.opt)
fit.lasso
res.lasso<- Y - fit.lasso
sigma.est<- mad(Y-fit.lasso)
beta.est<- as.numeric(model.est$beta)
gamma.est<-rep(0,n)

n.fold<- 5
n.cv <- n/n.fold
CV.error2<-CV.error<-rep(NA,length(lambda.gamma.try))
Y.pred.cv<-matrix(NA,nrow=length(Y),ncol=length(lambda.gamma.try))
Y.new<- Y
}

#OS lasso+
OS.lassoPLUS<- function(X,Y,lambda.lasso.try,lambda.gamma.try){
      #create simulation tracker
      #tracker <- as.vector(unlist(data$conditions)) 
      
      #print tracker of status
      #cat("n = " , tracker[1] , " , p = " , tracker[2] ,
      #    " , eta.x = " , tracker[3] , " , eta.y = " , tracker[4] ,
      #    " , g = " , tracker[5] , " , h = " , tracker[6] ,
      #    ";\n")
      
      #load X, Y, p, n
      #X <- data$X
      #Y <- data$Y
      #p <- data$conditions$p
      #n <- length(Y)
        
        
       n<-length(Y)
       n
       Y.orgn<- Y
       Y.orgn
       model.for.cv<- cv.glmnet(X, Y, family="gaussian",lambda=lambda.lasso.try)
       lambda.lasso.opt<- model.for.cv$lambda.min
       model.est<- glmnet(X,Y,family="gaussian",lambda=lambda.lasso.opt)
       fit.lasso<-  predict(model.est,X,s=lambda.lasso.opt)
       res.lasso<- Y - fit.lasso
       sigma.est<- mad(Y-fit.lasso)
       beta.est<- as.numeric(model.est$beta)
       gamma.est<-rep(0,n)
       
       K <- 5
       X.new <- kfold_subsetter(X , k = K , random = FALSE)
       Y.new <- cbind(Y , X.new[ , "subset"])
       n.cv <- n/ K
       CV.error2<-CV.error<-rep(NA,length(lambda.gamma.try))
       Y.pred.cv<-matrix(NA,nrow=length(Y),ncol=length(lambda.gamma.try))
       
       for (tt in 1:length(lambda.gamma.try))
       {
              gamma.est.cv<-rep(0,n-n.cv)
              
              for (jj in 1:K)
              {
                     subset <- unique(X.new[ , "subset"])[jj]
                     sample.out.index <- which(X.new[ , "subset"] == jj)
                     if(FALSE %in% (which(X.new[ , "subset"] == jj) == which(Y.new[ , 2] == jj))) {
                             stop("X and Y subsets do not match")
                     }  ##return error if the x and y subset indices do not match
                     X.train<- X.new[X.new[ , "subset"] != subset , -ncol(X.new)]
                     Y.train<- Y.new[Y.new[ , 2] != subset , 1]
                     X.test<- X.new[X.new[ , "subset"] == subset , -ncol(X.new)]
                     
                     model.train.temp<- glmnet(X.train,Y.train,family="gaussian",lambda=lambda.lasso.opt)
                     beta.pre<-beta.post<- as.numeric(model.train.temp$beta)
                     tol<-100; n.iter <- 0
                     while(tol>1e-6 & n.iter<100) 
                     {
                            resid.temp<- Y.train-X.train%*%beta.pre
                            nonzero<-which(abs(resid.temp)>=sigma.est*lambda.gamma.try[tt])
                            gamma.est.cv[nonzero]<- resid.temp[nonzero]
                            Y.train.new <- Y.train - gamma.est.cv
                            model.train.temp<- glmnet(X.train,Y.train.new,family="gaussian",lambda=lambda.lasso.opt)
                            beta.post <- as.numeric(model.train.temp$beta)
                            tol<- sum((beta.pre-beta.post)^2)
                            n.iter<- n.iter+1
                            beta.pre<-beta.post
                     }
                     
                     Y.pred.cv[sample.out.index,tt] <-X.test%*%beta.post
              }
              
              CV.error2[tt]<- mean((Y.pred.cv[,tt]-Y.orgn)^2)
              CV.error[tt]<- mean(abs(Y.pred.cv[,tt]-Y.orgn))
              
       }
       
       lambda.gamma.opt<- lambda.gamma.try[which.min(CV.error2)]
       
       model.opt<- glmnet(X,Y.orgn,family="gaussian",lambda=lambda.lasso.opt)
       beta.pre<- beta.post<- as.numeric(model.opt$beta)
       
       tol<-100; n.iter <- 0
       while(tol>1e-6 & n.iter<100) 
       {
              resid.opt<- Y.orgn-X%*%beta.pre
              nonzero<-which(abs(resid.opt)>=sigma.est*lambda.gamma.opt)
              gamma.est[nonzero]<- resid.opt[nonzero]
              Y.new2 <- Y.orgn - gamma.est
              model.opt<- glmnet(X,Y.new2,family="gaussian",lambda=lambda.lasso.opt)
              beta.post <-  as.numeric(model.opt$beta)
              tol<- mean((beta.pre-beta.post)^2)
              n.iter<- n.iter+1
              beta.pre<-beta.post
       }
       
       Y.fit<- X%*%beta.post
       
       #store number of nonzero coefs
       st.lad <- sum(beta.post)                                          # number nonzero
       
       #generate MSE and sd(MSE) for model
       mse.lad <- sum((Y - Y.fit) ^ 2) / (n - st.lad - 1)
       sd.mse.lad <- sd((Y - Y.fit) ^ 2 / (n - st.lad - 1))
       
       
       object<- list(coefficient = beta.post , 
                     fit = Y.fit , 
                     iter = n.iter , 
                     sigma.est = sigma.est , 
                     mpe = mse.lad , 
                     mpe.sd = sd.mse.lad ,
                     n.outlier = length(which(gamma.est != 0)) ,
                     gamma.est = gamma.est , 
                     lambda.opt = lambda.gamma.opt)
}




