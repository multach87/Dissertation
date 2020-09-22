#libraries
library(glmnet)
library(purrr)
library(magrittr)

#load data
#data.full <- readRDS()
#full.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/")
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
#testing10.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/testing10_data_091720.RData")

#winsorized function
winsorized <- function(x , a=1.5 , sigma=1) {
       s<-sigma
       newx<-x
       indp<-x>(a*s)
       newx[indp]<-(a*s)
       indn<- x<(a*-s)
       newx[indn]<- (-a*s)
       return(newx)}
#Huber lasso function
H.lasso.sim.fnct <- function(data){
        #create simulation tracker
        tracker <- as.vector(unlist(data$conditions)) 
        
        #print tracker of status
        cat("n = " , tracker[1] , " , p = " , tracker[2] ,
            " , eta.x = " , tracker[3] , " , eta.y = " , tracker[4] ,
            " , g = " , tracker[5] , " , h = " , tracker[6] ,
            ";\n")
        
        #load X, Y, n, p
        X <- data$X
        Y <- data$Y
        n <- length(Y)
        p <- data$conditions$p
        
        #set seed for generating initial lasso coefficients for weighting
        seed.lasso <- data$seeds[ , "seed.8"]
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
       
       sigma.est<- mean(Y.new- (X%*%beta.post)^2)
       Y.fit<- X%*%beta.post
       Y.res<- Y.new - Y.fit
       
       #store number of nonzero coefs
       st.lad <- sum(beta.post)                                          # number nonzero
       
       #generate MSE and sd(MSE) for model
       mse.Huber <- sum((Y - Y.fit) ^ 2) / (n - st.lad - 1)
       sd.mse.Huber <- sd((Y - Y.fit) ^ 2 / (n - st.lad - 1))
       
       #store lambda
       lambda.lasso.opt = model.est$lambda
       
       return(list(model = list(coefficient = beta.post , 
                                fit = Y.fit , 
                                iter = n.iter , 
                                sigma.est = sigma.est , 
                                mpe = mse.Huber , 
                                mpe.sd = sd.mse.Huber ,
                                lambda.opt = lambda.lasso.opt) , 
                   important = list(diagnostics = data.frame(cbind(data.seed = tracker[7] , 
                                                                   model.seed.lasso = seed.lasso)
                   ) , 
                   coefs = beta.post , 
                   info = data.frame(cbind(n = tracker[1] , 
                                           p = tracker[2] , 
                                           eta.x = tracker[3] , 
                                           eta.y = tracker[4] , 
                                           g = tracker[5] , 
                                           h = tracker[6] , 
                                           data.seed = tracker[7] ,
                                           model.seed.lasso = seed.lasso , 
                                           lambda.lasso = lambda.lasso.opt , 
                                           mpe = mse.Huber , 
                                           mpe.sd = sd.mse.Huber ,
                                           fpr = length(which(beta.post[c(5:p)] != 0)) / length(beta.post[c(5:p)]) , 
                                           fnr = length(which(beta.post[c(1:4)] == 0)) / length(beta.post[1:4]))
                   )
                   )
       )
       )

}

#run across full dataset
HuberLasso.debug <- debug.data %>%   
        map(safely(H.lasso.sim.fnct))
