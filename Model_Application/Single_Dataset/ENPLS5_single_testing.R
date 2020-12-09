#load libraries
library(glmnet)
library(magrittr)
library(purrr)
library(pls)

#load data
#data.half <- readRDS()
#full.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/fulldata_091620.RData")
#HD.data_DEBUG <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")
debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")

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

enpls <- function(X,y,avec,lamvec,cal,val,test,Ncomp,scale=F,yconv,PLSsc=F){
        #This function uses the elastic net regression to select variables, and PLS regression to model the data 
        #using the selected variables. 
        #Inputs:
        #X = matrix of data
        #y = vector of property
        #avec = vector of alpha values. alpha must be between 0 and 1. PLS-based outputs assume that avec[1]=0
        #lamvec = vector of lambda's in sequence to use for EN-PLS. By default, 100 are calculated. 
        #cal, val, test: indices for calibration, optimization, and test sets
        #Ncomp = Number of PLS components
        #scale = Is X autoscaled when EN is calculated?
        #PLSsc = If TRUE, Autoscales X when PLS regression is calculated
        #yconv = Converts units of y for calculation of RMSEP
        #Outputs: 
        #RMSEopt=Optimal RMSEopt for EN-PLS 
        #RMSEPtest=RMSEP for test set using optimal EN-PLS model
        #nvars=Number of variables selected by EN-PLS
        #vars=vecter of variable indices (channel numbers) for the variables selected by EN-PLS
        #vars.pos=vector of variable indices for selected variables with positive EN regression coefficients
        #vars.neg=vector of variable indices for selected variables with negative EN regression coefficients
        #optnlv=optimal number of latent variables chosen 
        #alflam= Values of alpha and lambda for best ENPLS model
        #PRESSopt=Predicted Residual Error Sum of Squares for optimization set
        #PRESStest=Predicted Residual Error Sum of Squares for test set
        #coefs=Elastic Net Regression coefficients for best EN-PLS model
        #For details on the EN-PLS Regression see "Using elastic net regression to perform spectrally relevant
        #variable selection" by C. Giglio and S.D. Brown, J. Chemometrics, 2018. 
        #R code by Cannon Giglio, 2016-2018, Brown Research Group, University of Delaware
        #if(min(avec)<0 | max(avec)>1){ stop("alpha values must be between 0 and 1")}
        #if(min(lamvec)<=0 | max(lamvec)>100){stop("lambda indices must be greater than 0 and less than/equal to 100")}
        y <- as.vector(y)
        if(length(y)!=nrow(X)){stop("Length of y does not Equal Number of Samples in X")}
        require(pls)
        require(glmnet)
        if(missing(yconv)){
                yconv <- rep(1,length(y))
        }
        lammat <- matrix(0, nrow=length(avec) , ncol=length(lamvec) ) #,dimnames=list(avec,lamvec)
        Nvarmat <- matrix(0, nrow=length(avec) , ncol=length(lamvec) ) #,dimnames=list(avec,lamvec)
        NLVmat <- matrix(0, nrow=length(avec) , ncol=length(lamvec) ) #,dimnames=list(avec,lamvec)
        RMSEPval <- array(100 , dim=c(length(avec),length(lamvec),Ncomp)) # ,dimnames=list(avec,lamvec,1:Ncomp)
        #biasarr <- array(100, dim=c(length(avec),length(lamvec),Ncomp),dimnames=list(avec,lamvec,1:Ncomp))
        #varexpl <- array(100, dim=c(length(avec),length(lamvec),2),dimnames=list(avec,lamvec,c("X","y")))
        X <- scale(X,center=colMeans(X[cal,]),scale=F)
        y <- scale(y,center=mean(y[cal]),scale=F)
        if(scale==T){    X0 <- scale(X,center=colMeans(X[cal,]),scale=apply(X[cal,],2,sd))  }
        else{X0 <- X}
        enp <- glmnet(X0[cal,],y[cal],alpha=0.5,family="gaussian",lambda = lamvec)
        i <- length(avec)
        for(j in 1:length(lamvec)){
                #if(lamvec[j]>length(enp$lambda)){ next }
                lammat[i,j] <- enp$lambda[j]
                enpcoef <- as.matrix(predict(enp , type = "coefficients" ,
                                             s = lamvec[j])[-1])
                cat("enpcoef = " , enpcoef , "\n")
                enpcs <- as.vector(apply(enpcoef, 2, function(z)sum(z!=0))) #number of active variables
                cat("enpcs = " , enpcs , "\n")
                if(enpcs == 0){
                        next
                } else{
                        colist <- rep(0,(enpcs))
                        colist <- which(enpcoef != 0) #vector/list of nonzero coefficients
                }
                Nvarmat[i,j] <- enpcs
                if(j==1){
                        whatcoef0 <- 0
                }
                whatcoef = which(enpcoef!=0)
                if(j>1 & length(whatcoef)==length(whatcoef0)){
                        if(isTRUE(any((whatcoef-whatcoef0)!=0))==F){
                                RMSEPval[i,j,] <- RMSEPval[i,(j-1),]
                                NLVmat[i,j] <- NLVmat[i,(j-1)]
                                #biasarr[i,j,] <- biasarr[i,(j-1),]
                                #varexpl[i,j,] <- varexpl[i,(j-1),]
                                next
                        }
                }
                #whatcoef0 <- whatcoef
                if(PLSsc==T){
                        enpdf.cal <- data.frame(y = y[cal], X = I(X0[cal,colist]))
                        df.val <- data.frame(y=y[val], X=I(X0[val,colist]))
                }
                else{
                        enpdf.cal <- data.frame(y = y[cal], X = I(X[cal,colist]))
                        df.val <- data.frame(y=y[val], X=I(X[val,colist]))
                }
                enpls <- plsr(y ~ X, data=enpdf.cal,subset=cal, ncomp=min(enpcs,Ncomp))
                enRMSEP <- rep(0,enpls$ncomp)
                valpreds <- predict(enpls, newdata = df.val, ncomp=1:min(enpcs,Ncomp))
                for(L in 1:enpls$ncomp){
                        enRMSEP[L] <- sqrt(mean(yconv[val]^2*(valpreds[,,L]-df.val$y)^2))
                        #biasarr[i,j,L] <- abs(sum(df.val[,1] - valpreds[,,L])/nrow(df.val))
                }
                cat("enRMSEP = " , enRMSEP , "\n")
                #cat("length(enRMSEP) = " , length(enRMSEP) , "\n")
                #cat("length(enRMSEP) < Ncomp =" , length(enRMSEP) < Ncomp , "\n")
                if(length(enRMSEP) < Ncomp){
                        #cat("we did it! \n")
                        enlen <- length(enRMSEP)
                        fill <- rep(100,(Ncomp-enlen))
                        RMSEPval[i,j,] <- c(enRMSEP,fill)
                } else { 
                        #cat("why are you here? \n")
                        RMSEPval[i,j,] <- enRMSEP 
                }
                optmin <- which.min(enRMSEP)
                cat("optmin = " , optmin , "\n")
                NLVmat[i,j] <- optmin
        }
        cat("RMSEPval = " , RMSEPval , "\n")
        BVval <- min(RMSEPval) #Best Validation RMSEP for Elastic Net
        cat("BVval = " , BVval , "\n")
        BVlamc <- which(RMSEPval==BVval,arr.ind=T)[1,1:3] #Indices 
        cat("BVlamc = " , BVlamc , "\n")
        nvars <- Nvarmat[BVlamc[1],BVlamc[2]]
        cat("nvars = " , nvars , "\n")
        RMSEopt <- RMSEPval[BVlamc[1],BVlamc[2],BVlamc[3]]
        cat("RMSEopt = " , RMSEopt , "\n")
        alflam <- c(avec[1],lammat[BVlamc[1],BVlamc[2]])
        #cat("lammat = " , lammat , "\n")
        cat("alflam = " , alflam , "\n")
        BVenp <- enp
        ENcoef <- predict(BVenp , type = "coefficients" , 
                          s = alflam[2])[-1]#as.matrix(coef(BVenp, s=BVenp$lambda[lamvec[BVlamc[2]]])[2:(ncol(X)+1),])
        cat("ENcoef = " , ENcoef , "\n")
        vars <- which(ENcoef != 0)
        cat("vars = " , vars , "\n")
        #vars.pos <- which(ENcoef > 0) 
        #vars.neg <- which(ENcoef < 0) 
        optnlv <- NLVmat[BVlamc[1],BVlamc[2]]
        #BVbias <- biasarr[BVlamc[1],BVlamc[2],BVlamc[3]]#,optnlv]
        #if(PLSsc==T){df <- data.frame(y = y, X = I(X0))}
        #else{    df <- data.frame(y = y, X = I(X))  }
        enpls.t <- plsr(y ~ X0[,vars] ,subset=cal, ncomp=optnlv) #, data=df
        valpreds <- predict(enpls.t, newdata = df[val,], ncomp=optnlv)
        PRESSopt= sum((valpreds-df$y[val])^2) #sum(yconv[val]^2*(valpreds-y[val])^2)
        testpreds <- predict(enpls.t, newdata = df[test,], ncomp=optnlv)
        PRESStest= sum(yconv[test]^2*(testpreds-df$y[test])^2) #sum((testpreds-df$y[test])^2)
        RMSEPtest <- sqrt(mean(yconv[test]^2*(testpreds[,,1]-df$y[test])^2))
        #testbias <- abs(sum(df$y[test] - testpreds[,,1])/length(test))
        #testSEP <- sqrt(RMSEPtest^2 - testbias^2)
        #ENSEP <- c(sqrt(BVval^2-BVbias^2),sqrt(RMSEPtest^2 - testbias^2))
        #return(list(RMSEPopt=RMSEPopt,RMSEPval=RMSEPval,nvars=Nvarmat,lamvals=lammat,NLV=NLVmat,
        #            vars=vars,vars.pos=vars.pos,vars.neg=vars.neg,alflam=alflam,nvars=nvars,
        #            optnlv=optnlv,ENcoef=ENcoef,RMSEPtest=RMSEPtest,testpreds=testpreds))
        return(list(RMSEopt=RMSEopt,RMSEPtest=RMSEPtest,nvars=nvars,vars=vars,
                    #vars.pos=vars.pos,vars.neg=vars.neg,optnlv=optnlv,alflam=alflam,
                    PRESSopt=PRESSopt,PRESStest=PRESStest,coefs=ENcoef))
}

enpls(X = data$X , y = data$Y , avec = avec , lamvec = lamvec ,
      cal = cbcal , val = cbopt , test = cbtst , Ncomp = ncol(data$X) , 
      scale = T , PLSsc = T)


#outside of sim function
data <- debug.data[[1]]

#inside sim function
#create vector of lambda values
lamvec <- exp(seq(log(0.01) , log(1400) , length.out = 100))
#create vector of alpha values
avec <- 0.5
#initialize X and Y
X <- data$X
y <- data$Y
#use kfold subsetter to create index for calibration/opitimization/testing sets (k = 3)
#initialize subset indices 
K <- 3 #since there are 3 sets: calibration, optimization, and testing
X.new <- kfold_subsetter(X , k = K , random = FALSE)
Y.new <- cbind(y , X.new[ , "subset"])
cbcal <- which(X.new[ , "subset"] == 1)
cbopt <- which(X.new[ , "subset"] == 2)
cbtst <- which(X.new[ , "subset"] == 3)
cal = cbcal ; val = cbopt ; test = cbtst ; Ncomp = 10
X <- scale(X,center=colMeans(X[cal,]),scale=F)
y <- scale(y,center=mean(y[cal]),scale=F)
X0 <- scale(X,center=colMeans(X[cal,]),scale=apply(X[cal,],2,sd))
enp.test <- cv.glmnet(X0[cal,] , y[cal] , alpha = 0.5 , family="gaussian" , 
                   lambda = lamvec)
lambda.adalasso.opt <- enp.test$lambda.min
best.adalasso.coefs <- predict(enp.test , type = "coefficients" ,
                               s = lambda.adalasso.opt)[-1]

enpcoef <- as.matrix(predict(enp.test , type = "coefficients" ,
                   s = 1400)[-1])





enpls5.model <- cv.glmnet(X , Y , family = "gaussian" ,
                          lambda = lamvec , alpha = 0.5 ,
                          penalty.factor = 1 / abs(best.ridge.coefs)^nu.try[i])


#adaptive lasso function with two-way CV for selecting both lambda and nu/gamma
enpls5.sim.fnct <- function(data) { 
       #create simulation tracker
       tracker <- as.vector(unlist(data$conditions)) 
       #print tracker of status
       cat("n = " , tracker[1] , " , p = " , tracker[2] ,
           " , eta.x = " , tracker[3] , " , eta.y = " , tracker[4] ,
           " , g = " , tracker[5] , " , h = " , tracker[6] ,
           ";\n")
       #load X, Y, and p
       X <- data$X
       Y <- data$Y
       p <- data$conditions$p
       #seed.ridge <- data$seeds[ , "seed.14"]
       #set.seed(seed.ridge)
       #ridge coefs for weighting
       lamvec <- exp(seq(log(0.01) , log(1400) , length.out = 100))
       ridge.model <- cv.glmnet(x = X , y = Y , lambda = lamvec , alpha = 0)
       lambda.ridge.opt <- ridge.model$lambda.min
       best.ridge.coefs <- predict(ridge.model , type = "coefficients" ,
                                   s = lambda.ridge.opt)[-1]
       ##grid of nu/gamma values to try
       nu.try <- exp(seq(log(0.01) , log(10) , length.out = 100))
       #seed.pre.nu <- data$seeds[ , "seed.15"]
       #set.seed(seed.pre.nu)
       #seed.nu <- sample(rnorm(n = 1000000000) , size = length(nu.try) , replace = FALSE)
       ##initialize list of best enpls5 results from each nu/gamma
       enpls5.nu.cv <- list()
       for(i in 1:length(nu.try)) {
              #seed <- seed.nu[i]
              #set.seed(seed)
              #single adaptive lasso run with ridge weighting and nu = 1
              enpls5.model <- cv.glmnet(X , Y , family = "gaussian" ,
                                          lambda = lamvec , alpha = 0.5 ,
                                          penalty.factor = 1 / abs(best.ridge.coefs)^nu.try[i])
              lambda.enpls5.opt <- enpls5.model$lambda.min
              best.enpls5.coefs <- predict(enpls5.model , type = "coefficients" ,
                                             s = lambda.enpls5.opt)[-1]
              enpls5.nu.cv[[i]] <- list(model = list(full.model = enpls5.model , 
                                                       lambda = lambda.enpls5.opt , 
                                                       coefs = best.enpls5.coefs) , 
                                          metrics_and_info = list(#model.seed.ridge = seed.ridge ,
                                                                  #model.seed.prenu = seed.pre.nu , 
                                                                  #model.seed.nu = seed ,
                                                                  ridge.coefs = best.ridge.coefs ,
                                                                  weights = 1 / abs(best.ridge.coefs)^nu.try[i] , 
                                                                  nu = nu.try[i] , 
                                                                  lambda = lambda.enpls5.opt , 
                                                                  coefs = best.enpls5.coefs , 
                                                                  mpe = enpls5.model$cvm[which(enpls5.model$lambda == lambda.enpls5.opt)] , 
                                                                  mpe.sd = enpls5.model$cvsd[which(enpls5.model$lambda == lambda.enpls5.opt)] , 
                                                                  fpr = length(which(best.enpls5.coefs[c(5:p)] != 0)) / length(best.enpls5.coefs[c(5:p)]) , 
                                                                  fnr = length(which(best.enpls5.coefs[c(1:4)] == 0)) / length(best.enpls5.coefs[1:4])))
       }
       #find minimizing nu/gamma
       enpls5.nu.cv.mpe <- numeric()
       #enpls5.seeds.ridge <- numeric()
       #enpls5.seeds.prenu <- numeric()
       #enpls5.seeds.nu <- numeric()
       for(i in 1:length(enpls5.nu.cv)) {
              enpls5.nu.cv.mpe[i] <- enpls5.nu.cv[[i]]$metrics_and_info$mpe
              #enpls5.seeds.ridge[i] <- enpls5.nu.cv[[i]]$metrics_and_info$model.seed.ridge
              #enpls5.seeds.prenu[i] <- enpls5.nu.cv[[i]]$metrics_and_info$model.seed.prenu
              #enpls5.seeds.nu[i] <- enpls5.nu.cv[[i]]$metrics_and_info$model.seed.nu
       }

       #return(enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]])
       #store BEST enpls5 result plus all seeds
       ###below is used to check that seeds are regenerated properly and not uniform
       return(list(mpes = enpls5.nu.cv.mpe , 
                   #seeds.ridge = enpls5.seeds.ridge , 
                   #seeds.prenu = enpls5.seeds.prenu , 
                   #seeds.nu = enpls5.seeds.nu ,  
                   model = enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]] , 
                   important = list(diagnostics = data.frame(cbind(data.seed = tracker[7])) ,
                                                                   #model.seed.ridge = enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge ,
                                                                   #model.seed.prenu = enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                                   #model.seed.nu = enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]]$metrics_and_info$model.seed.nu)) , 
                                    coefs = enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]]$metrics_and_info$coefs , 
                                    weights = enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]]$metrics_and_info$weights ,
                                    info = data.frame(cbind(n = tracker[1] , 
                                                            p = tracker[2] , 
                                                            eta.x = tracker[3] , 
                                                            eta.y = tracker[4] , 
                                                            g = tracker[5] , 
                                                            h = tracker[6] , 
                                                            data.seed = tracker[7] ,
                                                            #model.seed.ridge = enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge , 
                                                            #model.seed.prenu = enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                            #model.seed.nu = enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]]$metrics_and_info$model.seed.nu , 
                                                            alpha = 0.5 , 
                                                            lambda = enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]]$metrics_and_info$lambda ,
                                                            nu = enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]]$metrics_and_info$nu ,
                                                            mpe = enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]]$metrics_and_info$mpe , 
                                                            mpe.sd = enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]]$metrics_and_info$mpe.sd , 
                                                            fpr = enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]]$metrics_and_info$fpr , 
                                                            fnr = enpls5.nu.cv[[which.min(enpls5.nu.cv.mpe)]]$metrics_and_info$fnr
                                                            )
                                                      )
                                    )
                   )
              )
}


#run across debug dataset
enpls5.HD.debug <- HD.data_DEBUG %>%   
       map(safely(enpls5.sim.fnct))

saveRDS(enpls5.HD.debug , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/enpls5_HD_DEBUG.RData")
