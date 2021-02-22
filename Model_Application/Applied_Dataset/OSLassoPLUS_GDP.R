#load libraries
library(glmnet)
library(magrittr)
library(purrr)

#load data
#data.full <- readRDS()
GDP <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/gdp_split.RData")

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


#oslassoPLUS application function
oslassoPLUS.sim.fnct <- function(data) {
        #create simulation tracker
        #tracker <- as.vector(unlist(data$conditions)) 
        
        #print tracker of status
        cat("iteration = " , data$track , ";\n")
      
       #load X, Y, p, n
       X <- as.matrix(data$train[ , -c(1 , 15)])
       #cat("X = " , X , "\n")
       #cat("class(X) = " , class(X) , "\n")
       Y <- data$train$y.net
       Y.orgn<- Y
       #cat("class(Y) = " , class(Y) , "\n")
       n <- length(Y)
       lambda.lasso.try <- seq(log(0.01) , log(1400) , length.out = 100)
       lambda.lasso.try <- exp(lambda.lasso.try)
       lambda.gamma.try <- exp(seq(log(1) , log(1400) , length.out = 100))
       
       #determine initial oslassoPLUS coefs
       model.for.cv<- cv.glmnet(X, Y, family="gaussian",lambda=lambda.lasso.try)
       lambda.oslassoPLUS.opt<- model.for.cv$lambda.min
       model.est<- glmnet(X,Y,family="gaussian",lambda=lambda.oslassoPLUS.opt)
       fit.oslassoPLUS<-  predict(model.est,X,s=lambda.oslassoPLUS.opt)
       res.oslassoPLUS<- Y - fit.oslassoPLUS
       sigma.est<- mad(Y-fit.oslassoPLUS)
       beta.est<- as.numeric(model.est$beta)
       gamma.est<-rep(0,n)
       
       #initialize subset indices 
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
           
           model.train.temp<- glmnet(X.train,Y.train,family="gaussian",lambda=lambda.oslassoPLUS.opt)
           beta.pre<-beta.post<- as.numeric(model.train.temp$beta)
           tol<-100; n.iter <- 0
           while(tol>1e-6 & n.iter<100) 
           {
             resid.temp<- Y.train-X.train%*%beta.pre
             nonzero<-which(abs(resid.temp)>=sigma.est*lambda.gamma.try[tt])
             gamma.est.cv[nonzero]<- resid.temp[nonzero]
             #cat("length(Y.train) = " , length(Y.train) , "\n")
             #cat("length(gamma.est.cv) = " , length(gamma.est.cv) , "\n")
             Y.train.new <- Y.train - gamma.est.cv
             model.train.temp<- glmnet(X.train,Y.train.new,family="gaussian",lambda=lambda.oslassoPLUS.opt)
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
       
       model.opt<- glmnet(X,Y.orgn,family="gaussian",lambda=lambda.oslassoPLUS.opt)
       beta.pre<- beta.post<- as.numeric(model.opt$beta)
       
       tol<-100; n.iter <- 0
       while(tol>1e-6 & n.iter<100) 
       {
         resid.opt<- Y.orgn-X%*%beta.pre
         nonzero<-which(abs(resid.opt)>=sigma.est*lambda.gamma.opt)
         gamma.est[nonzero]<- resid.opt[nonzero]
         Y.new2 <- Y.orgn - gamma.est
         model.opt<- glmnet(X,Y.new2,family="gaussian",lambda=lambda.oslassoPLUS.opt)
         beta.post <-  as.numeric(model.opt$beta)
         tol<- mean((beta.pre-beta.post)^2)
         n.iter<- n.iter+1
         beta.pre<-beta.post
       }
       n.coefs <- sum(beta.post != 0)
       
       #specify test data
       test.X <- as.matrix(data$test[ , -c(1 , 15)])
       #cat("class(test.X) = " , class(test.X) , "\n")
       test.Y <- data$test$y.net
       
       #apply to test set
       pred.y <- test.X %*% beta.post
       #cat("pred.y = " , pred.y , "\n")
       resid <- pred.y - test.Y
       resid.sq <- resid^2
       sum.resid.sq <- sum(resid.sq)
       mse <- sum.resid.sq / n

       #initialize important info dataframe
       #put conditions, model info, and metrics into list
       return(list(model = list(full.model = model.opt , 
                                iter = n.iter , 
                                sigma.est = sigma.est , 
                                n.outlier = length(which(gamma.est != 0)) ,
                                gamma.est = gamma.est , 
                                lambda.oslassoPLUS.opt = lambda.oslassoPLUS.opt ,
                                lambda.gamma.opt = lambda.gamma.opt , 
                                coefs = beta.post) , 
                   metrics = list(mse = mse , 
                                   n.coefs = n.coefs
                                  )
                   )
       )

}




#run across full dataset
oslassoPLUS.full <- GDP %>%   
       map(safely(oslassoPLUS.sim.fnct))

saveRDS(oslassoPLUS.full , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Applied_Storage/oslassoPLUS_GDP.RData")


