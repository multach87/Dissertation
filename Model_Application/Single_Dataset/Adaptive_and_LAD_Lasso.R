#load libraries
library(quantreg)

#lsa.linear.txt
ladlasso.rq <- function(x , y , adalass) {
        n <- length(y)
        grid <- seq(log(0.01) , log(1400) , length.out = 100)
        grid <- exp(grid)
        rqob <- rq(y ~ 0 + x)
        BIC <- rep(0 , 100)
        weights <- 1 / abs(rqob$coef)
        
        for (k in 1:100){
                rqfit <- rq.fit.lasso(x , y , lambda = grid[k] * weights)
                betalad_tmp <- rqfit$coef
                betalad_tmp <- betalad_tmp * (betalad_tmp > 1e-8)
                mse <- mean(abs(rqfit$resi))
                mdsize <- length(which(betalad_tmp != 0))
                BIC[k] <- log(mse) + mdsize * log(n) / n
        }
        
        step <- which.min(BIC) 
        betalad <- rq.fit.lasso(x , y , lambda = grid[step] * weights)$coef
        ladlasso <- betalad * (betalad > 1e-8)
        
        #testing ladlasso mse
        coeff.lad <- c(adalass$intercept , ladlasso)
        coeff2.lad <- ladlasso              # get rid of intercept
        pred.lad <- x %*% coeff2.lad + coeff.lad[1]
        st.lad <- sum(coeff2.lad != 0)                                          # number nonzero
        mse.lad <- sum((y - pred.lad) ^ 2) / (n - st.lad - 1)
        return(list(fit = pred.lad , st = st.lad , mse = mse.lad , 
                    coeff = coeff2.lad , intercept = coeff.lad[1] , weights = weights))
}
ladlasso.ridge <- function(x , y , adalass) {
        n <- length(y)
        grid <- seq(log(0.01) , log(1400) , length.out = 100)
        grid <- exp(grid)
        rqob <- rq(y ~ 0 + x)
        BIC <- rep(0 , 100)
        weights <- 1 / abs(rqob$coef)
        
        for (k in 1:100){
                rqfit <- rq.fit.lasso(x , y , lambda = grid[k] * weights)
                betalad_tmp <- rqfit$coef
                betalad_tmp <- betalad_tmp * (betalad_tmp > 1e-8)
                mse <- mean(abs(rqfit$resi))
                mdsize <- length(which(betalad_tmp != 0))
                BIC[k] <- log(mse) + mdsize * log(n) / n
        }
        
        step <- which.min(BIC) 
        betalad <- rq.fit.lasso(x , y , lambda = grid[step] * weights)$coef
        ladlasso <- betalad * (betalad > 1e-8)
        
        #testing ladlasso mse
        coeff.lad <- c(adalass$intercept , ladlasso)
        coeff2.lad <- ladlasso              # get rid of intercept
        pred.lad <- x %*% coeff2.lad + coeff.lad[1]
        st.lad <- sum(coeff2.lad != 0)                                          # number nonzero
        mse.lad <- sum((y - pred.lad) ^ 2) / (n - st.lad - 1)
        return(list(fit = pred.lad , st = st.lad , mse = mse.lad , 
                    coeff = coeff2.lad , intercept = coeff.lad[1] , weights = weights))
}

#load data
#data.full <- readRDS()
single.data <- readRDS("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Dissertation_Git/Data_Generation/Data_Storage/single_data_091520.RData")
X <- single.data[[1]]$X
Y <- single.data[[1]]$Y
p <- single.data[[1]]$conditions$p

#apply LAD/Adaptive lasso
set.seed(1)
adlasso.rq <- lsa.linear(X , Y) #FIX WEIGHTS


lambda.lasso.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
nu.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
lasso.model <- cv.glmnet(X , Y , family = "gaussian" ,
                         lambda = lambda.lasso.try)
lambda.lasso.opt <- lasso.model$lambda.min
lasso.coefs <- predict(lasso.model , type = "coefficients" ,
                       s = lambda.lasso.opt)[2 : (p + 1)]
adlasso2 <- cv.glmnet(X , Y , family = "gaussian" ,
                      lambda = lambda.lasso.try , penalty.factor = 1 / abs(lasso.coefs)^nu.try)
adlasso3 <- cv.glmnet(X , Y , family = "gaussian" ,
                                  lambda = lambda.lasso.try)
lambda.adlasso.opt <- adlasso2$lambda.min
adlasso.coefs <- predict(adlasso2 , type = "coefficients" ,
                       s = lambda.adlasso.opt)[2 : (p + 1)]


ladlasso <- lad.lasso(x = X , y = Y , adalass = adlasso2)
adlass.model <- list(full.model = adlasso , lambda = lambda.lasso.opt , coefs = lasso.coefs)

#Metrics
which(lasso.model$lambda == lambda.lasso.opt)
lasso.err <- list(mpe = lasso.model$cvm[which(lasso.model$lambda == lambda.lasso.opt)] , 
                  mpe.sd = lasso.model$cvsd[which(lasso.model$lambda == lambda.lasso.opt)])
lasso.err
metrics <- list(mpe = lasso.model$cvm[which(lasso.model$lambda == lambda.lasso.opt)] , 
                mpe.sd = lasso.model$cvsd[which(lasso.model$lambda == lambda.lasso.opt)] , 
                fpr = length(which(lasso.coefs[c(5:p)] != 0)) / length(lasso.coefs[c(5:p)]) , 
                fnr = length(which(lasso.coefs[c(1:4)] == 0)) / length(lasso.coefs[1:4]))



