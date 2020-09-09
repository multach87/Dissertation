#load libraries
library(quantreg)

#lsa.linear.txt
lsa.linear<-function(x,y){
       require(lars)
       
       ## Least square approximation. This version Oct 19, 2006
       
       ## Reference Wang, H. and Leng, C. (2006) and Efron et al. (2004).
       
       ##
       
       ## Written by Chenlei Leng
       
       ## Comments and suggestions are welcome
       
       ##
       
       ## Input
       
       ## obj: lm/glm/coxph or other object
       
       ##
       
       ## Output
       
       ## beta.ols: the MLE estimate
       
       ## beta.bic: the LSA-BIC estimate
       
       ## beta.aic: the LSA-AIC estimate
       
       lsa <- function(obj)
              
       { 
              
              intercept <- attr(obj$terms,'intercept')
              
              if(class(obj)[1]=='coxph') intercept <- 0
              
              
              
              n <- length(obj$residuals)
              
              
              
              Sigma <- vcov(obj)
              #cat("SigmaOrig = " , Sigma , "\n")
              SI <- solve(Sigma)
              #cat("SI = " , SI , "\n")
              
              beta.ols <- coef(obj)
              
              l.fit <- lars.lsa(SI, beta.ols, intercept, n)
              
              
              
              t1 <- sort(l.fit$BIC, ind=T)
              
              t2 <- sort(l.fit$AIC, ind=T)
              
              beta <- l.fit$beta
              
              if(intercept) {
                     
                     beta0 <- l.fit$beta0+beta.ols[1]
                     
                     beta.bic <- c(beta0[t1$ix[1]],beta[t1$ix[1],])
                     
                     beta.aic <- c(beta0[t2$ix[1]],beta[t2$ix[1],])
                     
              }
              
              else {
                     
                     beta0 <- l.fit$beta0
                     
                     beta.bic <- beta[t1$ix[1],]
                     
                     beta.aic <- beta[t2$ix[1],]
                     
              }
              
              
              
              obj <- list(beta.ols=beta.ols, beta.bic=beta.bic,
                          
                          beta.aic = beta.aic)
              
              obj
              
       }
       
       
       
       
       
       ###################################
       
       ## lars variant for LSA
       
       lars.lsa <- function (Sigma0, b0, intercept,  n,
                             
                             type = c("lasso", "lar"),
                             
                             eps = .Machine$double.eps,max.steps) 
              
       {
              
              type <- match.arg(type)
              
              TYPE <- switch(type, lasso = "LASSO", lar = "LAR")
              
              
              
              n1 <- dim(Sigma0)[1]
              
              
              
              ## handle intercept
              
              if (intercept) {
                     #cat("intercept = TRUE" , "\n")
                     #cat("Sigma0 = " , Sigma0 , "\n")
                     a11 <- Sigma0[1,1]
                     #cat("a11 = " , a11 , "\n")
                     a12 <- Sigma0[2:n1,1]
                     #cat("a12 = " , a12 , "\n")
                     a22 <- Sigma0[2:n1,2:n1]
                     ##cat("a22 = " , a22 , "\n")
                     Sigma <- a22-outer(a12,a12)/a11
                     #cat("tail(Sigma) = " , tail(Sigma , 10) , "\n")
                     b <- b0[2:n1]
                     #cat("b0 = " , b0 , "\n")
                     #cat("b = " , b , "\n")
                     beta0 <- crossprod(a12,b)/a11
                     
              }
              
              else {
                     #cat("intercept = FALSE" , "\n")
                     Sigma <- Sigma0
                     
                     b <- b0
                     
              }
              
              
              
              Sigma <- diag(abs(b))%*%Sigma%*%diag(abs(b))
              
              b <- sign(b)
              
              
              
              nm <- dim(Sigma)
              
              m <- nm[2]
              
              #im <- seq(m)
              im <- inactive <- seq(m)
              
              #cat("b = " , b , "\n")
              #cat("length.b = " , length(b) , "\n")
              #cat("Sigma = " , Sigma , "\n")
              #cat("length.Sigma = " , length(Sigma) , "\n")
              Cvec <- drop(t(b)%*%Sigma)
              #cat("Cvec = " , Cvec , "\n")
              ssy <- sum(Cvec*b)
              
              if (missing(max.steps)) 
                     
                     max.steps <- 8 * m
              
              beta <- matrix(0, max.steps + 1, m)
              
              Gamrat <- NULL
              
              arc.length <- NULL
              
              R2 <- 1
              
              RSS <- ssy
              
              first.in <- integer(m)
              
              active <- NULL
              
              actions <- as.list(seq(max.steps))
              
              drops <- FALSE
              
              Sign <- NULL
              
              R <- NULL
              
              k <- 0
              
              ignores <- NULL
              
              
              
              while ((k < max.steps) & (length(active) < m)) {
                     
                     action <- NULL
                     
                     k <- k + 1
                     #cat("inactive = " , inactive , "\n")
                     C <- Cvec[inactive]
                     #cat("C0 = " , C , "\n")
                     Cmax <- max(abs(C))
                     
                     if (!any(drops)) {
                            #cat("C1 = " , C , "\n")
                            new <- abs(C) >= Cmax - eps
                            #cat("new1 = " , new , "\n")
                            
                            C <- C[!new]
                            #cat("C2 = " , C , "\n")
                            new <- inactive[new]
                            #cat("new2 = " , new , "\n")
                            
                            for (inew in new) {
                                   #cat("inew = " , inew , "\n")
                                   R <- updateR(Sigma[inew, inew], R, drop(Sigma[inew, active]),
                                                
                                                Gram = TRUE,eps=eps)
                                   
                                   if(attr(R, "rank") == length(active)) {
                                          
                                          ##singularity; back out
                                          
                                          nR <- seq(length(active))
                                          
                                          R <- R[nR, nR, drop = FALSE]
                                          
                                          attr(R, "rank") <- length(active)
                                          
                                          ignores <- c(ignores, inew)
                                          
                                          action <- c(action,  - inew)
                                          
                                   }
                                   
                                   else {
                                          #cat("first.in = " , "\n" , first.in , "\n")
                                          #cat("inew = " , "\n" , inew , "\n")
                                          if(first.in[inew] == 0)
                                                 
                                          {first.in[inew] <- k}
                                          
                                          active <- c(active, inew)
                                          
                                          Sign <- c(Sign, sign(Cvec[inew]))
                                          
                                          action <- c(action, inew)
                                          
                                   }
                                   
                            }
                            
                     }
                     
                     else action <- -dropid
                     
                     Gi1 <- backsolve(R, backsolvet(R, Sign))
                     
                     dropouts <- NULL
                     
                     A <- 1/sqrt(sum(Gi1 * Sign))
                     
                     w <- A * Gi1
                     
                     
                     
                     if (length(active) >= m) {
                            
                            gamhat <- Cmax/A      
                            
                     }
                     
                     else {        
                            
                            a <- drop(w %*% Sigma[active, -c(active,ignores), drop = FALSE])
                            
                            gam <- c((Cmax - C)/(A - a), (Cmax + C)/(A + a))
                            
                            gamhat <- min(gam[gam > eps], Cmax/A)
                            
                     }
                     
                     if (type == "lasso") {
                            
                            dropid <- NULL
                            
                            b1 <- beta[k, active]
                            
                            z1 <- -b1/w
                            
                            zmin <- min(z1[z1 > eps], gamhat)
                            
                            # cat('zmin ',zmin, ' gamhat ',gamhat,'\n') 
                            
                            if (zmin < gamhat) {
                                   
                                   gamhat <- zmin
                                   
                                   drops <- z1 == zmin
                                   
                            }
                            
                            else drops <- FALSE
                            
                     }
                     
                     beta[k + 1, ] <- beta[k, ]
                     
                     beta[k + 1, active] <- beta[k + 1, active] + gamhat * w
                     
                     
                     
                     Cvec <- Cvec - gamhat * Sigma[, active, drop = FALSE] %*% w   
                     
                     Gamrat <- c(Gamrat, gamhat/(Cmax/A))
                     
                     
                     
                     arc.length <- c(arc.length, gamhat)
                     
                     if (type == "lasso" && any(drops)) {
                            
                            dropid <- seq(drops)[drops]
                            
                            for (id in rev(dropid)) {
                                   
                                   R <- downdateR(R,id)
                                   
                            }
                            
                            dropid <- active[drops]
                            
                            beta[k + 1, dropid] <- 0
                            
                            active <- active[!drops]
                            
                            Sign <- Sign[!drops]
                            
                     }
                     
                     
                     
                     actions[[k]] <- action
                     
                     inactive <- im[-c(active)]
                     
              }
              
              beta <- beta[seq(k + 1), ]
              
              
              
              dff <- b-t(beta)
              
              
              
              RSS <- diag(t(dff)%*%Sigma%*%dff)
              
              
              
              if(intercept)
                     
                     beta <- t(abs(b0[2:n1])*t(beta))
              
              else
                     
                     beta <- t(abs(b0)*t(beta))
              
              
              
              if (intercept) {
                     
                     beta0 <- as.vector(beta0)-drop(t(a12)%*%t(beta))/a11
                     
              }
              
              else {
                     
                     beta0 <- rep(0,k+1)
                     
              }
              
              dof <- apply(abs(beta)>eps,1,sum)
              
              BIC <- RSS+log(n)*dof
              
              AIC <- RSS+2*dof
              
              object <- list(AIC = AIC, BIC = BIC, 
                             
                             beta = beta, beta0 = beta0)
              
              object
              
       }
       
       
       
       ##Note that rq() object implemented the coef()
       
       ##but without vcov() implementation. We provide
       
       ##a rather simple implementation here.
       
       ##This part is written by Hansheng Wang.
       
       vcov.rq <- function(object,...)
              
       {
              
              q=object$tau
              
              x=as.matrix(object$x)
              
              resid=object$residuals
              
              f0=density(resid,n=1,from=0,to=0)$y
              
              COV=q*(1-q)*solve(t(x)%*%x)/f0^2
              
              COV
              
       }
       
       
       # adaptive lasso for linear reg, tuning parameter by bic 
       # calls software from Wang and Leng (2007, JASA).
       ok<-complete.cases(x,y)
       x<-x[ok,]                            # get rid of na's
       y<-y[ok]                             # since regsubsets can't handle na's
       m<-ncol(x)
       n<-nrow(x)
       as.matrix(x)->x
       lm(y~x)->out
       #cat("x = " , x , "\n")
       #cat("y = " , y , "\n")
       #cat("test5, i = " , i , "\n")
       lsa(out)->out.lsa    #EVERYTHING HAPPENS HERE
       #cat("test6, i = " , i , "\n")
       coeff<-out.lsa$beta.bic
       coeff2<-coeff[2:(m+1)]               # get rid of intercept
       pred<-x%*%coeff2+coeff[1]
       st<-sum(coeff2 !=0)                                          # number nonzero
       mse<-sum((y-pred)^2)/(n-st-1)
       if(st>0) x.ind<-as.vector(which(coeff2 !=0)) else x.ind<-0
       return(list(fit=pred,st=st,mse=mse,x.ind=x.ind,coeff=coeff2,intercept=coeff[1]))
}

#load data
#data.full <- readRDS()
single.data <- data.full[[1]]
X <- single.data$X
Y <- single.data$Y

#apply LAD/Adaptive lasso
set.seed(1)
object <- lsa.linear(X , Y)
adlasso <- object$coef
n <- length(Y)
grid=seq(log(0.01),log(1400),length.out=100) 
grid=exp(grid)
rqob=rq(Y~0+X)
BIC <- rep(0 , 100)
weights <- 1/abs(rqob$coef)
for ( i in 1:100){
       #cat("i = " , i , "\n")
       rqfit=rq.fit.lasso(X,Y,lambda=grid[i]*weights);
       betalad_tmp=rqfit$coef;
       betalad_tmp=betalad_tmp*(betalad_tmp>1e-8);
       mse=mean(abs(rqfit$resi));
       mdsize=length(which(betalad_tmp!=0));
       BIC[i]=log(mse)+mdsize*log(n)/n;
}
step=which.min(BIC);
betalad=rq.fit.lasso(X,Y,lambda=grid[step]*weights)$coef;
ladlasso=betalad*(betalad>1e-8)
