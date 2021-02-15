#load libraries
library(quantreg)
library(glmnet)
library(magrittr)
library(purrr)

#load data
HD.ex1ey2 <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_ex1ey2.RData")


#LAD lasso function with two-way CV for selecting both lambda and nu/gamma
ladlasso.sim.fnct <- function(data) {
  #create simulation tracker
  tracker <- as.vector(unlist(data$conditions)) 
  
  #print tracker of status
  cat("n = " , tracker[1] , " , p = " , tracker[2] ,
      " , eta.x = " , tracker[3] , " , eta.y = " , tracker[4] ,
      " , g = " , tracker[5] , " , h = " , tracker[6] ,
      ";\n")
  
  #load X, Y, p, n
  X <- data$X
  Y <- data$Y
  p <- data$conditions$p
  n <- length(Y)
  
  #set seed for generating ridge coefficients for weighting
  #seed.ridge <- data$seeds[ , "seed.4"]
  #set.seed(seed.ridge)
  
  #set possible lambda and nu/gamma values
  lambda.try <- seq(log(0.01) , log(1400) , length.out = 100)
  lambda.try <- exp(lambda.try)
  nu.try <- exp(seq(log(0.01) , log(10) , length.out = 100))
  
  #set seed for generating nu/gamma for weighting
  #seed.pre.nu <- data$seeds[ , "seed.5"]
  #set.seed(seed.pre.nu)
  #seed.nu <- sample(rnorm(n = 1000000000) , size = length(nu.try) , replace = FALSE)
  
  #find ridge coefs for adaptive weighting
  lambda.try <- exp(seq(log(0.01) , log(1400) , length.out = 100))
  ridge.model <- cv.glmnet(x = X , y = Y , lambda = lambda.try , alpha = 0)
  lambda.ridge.opt <- ridge.model$lambda.min
  best.ridge.coefs <- predict(ridge.model , type = "coefficients" ,
                              s = lambda.ridge.opt)[-1]
  
  ##initialize list of best ladlasso results from each nu/gamma
  ladlasso.nu.cv <- list()
  
  #loop to generate results from each nu/gamma
  for(i in 1:length(nu.try)) {
    #set seed for random process
    #seed <- seed.nu[i]
    #set.seed(seed)
    
    #set adaptive weights
    weights <- 1 / (abs(best.ridge.coefs)^nu.try[i])
    
    #create empty vector for BIC values for each possible lambda value
    BIC <- rep(0 , 100)
    
    #generate model for each possible lambda value
    for (k in 1:100){
      rqfit <- rq.fit.lasso(X , Y , lambda = lambda.try[k] * weights)
      betalad_tmp <- rqfit$coef
      betalad_tmp <- betalad_tmp * (betalad_tmp > 1e-8)
      mse <- mean(abs(rqfit$resi))
      mdsize <- length(which(betalad_tmp != 0))
      BIC[k] <- log(mse) + mdsize * log(n) / n
    }
    
    #indicator for BIC-minimizing lambda/model
    step <- which.min(BIC) 
    
    #generate LAD lasso coefficients for minimizing lambda/model
    betalad <- rq.fit.lasso(X , Y , lambda = lambda.try[step] * weights)$coef
    ladlasso <- betalad * (betalad > 1e-8)
    
    #store best lambda for given nu/gamma
    lambda.ladlasso.opt <- lambda.try[step]
    
    #store coefficients
    coeff2.lad <- ladlasso              # get rid of intercept
    
    #generate y-hats for each observation
    pred.lad <- X %*% coeff2.lad #+ coeff.lad[1]
    
    #store number of nonzero coefs
    st.lad <- sum(coeff2.lad != 0)                                          # number nonzero
    
    #generate MSE and sd(MSE) for model
    mse.lad <- sum((Y - pred.lad) ^ 2) / (n - st.lad - 1)
    sd.mse.lad <- sd((Y - pred.lad) ^ 2 / (n - st.lad - 1))
    
    #save list of all info from best model
    ladlasso.nu.cv[[i]] <- list(other.info = list(fit = pred.lad , 
                                                  st = st.lad) , 
                                metrics_and_info = list(BIC.min = min(BIC) , 
                                                        which.BIC.min = step , 
                                                        #model.seed.ridge = seed.ridge ,
                                                        #model.seed.prenu = seed.pre.nu , 
                                                        #model.seed.nu = seed ,
                                                        ridge.coefs = best.ridge.coefs ,
                                                        weights = weights , 
                                                        nu = nu.try[i] , 
                                                        lambda = lambda.try[step] , 
                                                        coefs = coeff2.lad , 
                                                        mpe = mse.lad , 
                                                        mpe.sd = sd.mse.lad ,
                                                        fpr = length(which(coeff2.lad[c(5:p)] != 0)) / length(coeff2.lad[c(5:p)]) , 
                                                        fnr = length(which(coeff2.lad[c(1:4)] == 0)) / length(coeff2.lad[1:4])))
    
  }
  #find/store minimizing nu/gamma, seeds, minimized BIC/step
  ladlasso.nu.cv.mpe <- numeric()
  #ladlasso.seeds.ridge <- numeric()
  #ladlasso.seeds.prenu <- numeric()
  #ladlasso.seeds.nu <- numeric()
  ladlasso.BIC.mins <- numeric()
  ladlasso.which.BIC.mins <- numeric()
  for(i in 1:length(ladlasso.nu.cv)) {
    ladlasso.nu.cv.mpe[i] <- ladlasso.nu.cv[[i]]$metrics_and_info$mpe
    #ladlasso.seeds.ridge[i] <- ladlasso.nu.cv[[i]]$metrics_and_info$model.seed.ridge
    #ladlasso.seeds.nu[i] <- ladlasso.nu.cv[[i]]$metrics_and_info$model.seed.nu
    #ladlasso.seeds.prenu[i] <- ladlasso.nu.cv[[i]]$metrics_and_info$model.seed.prenu
    ladlasso.BIC.mins[i] <- ladlasso.nu.cv[[i]]$metrics_and_info$BIC.min
    ladlasso.which.BIC.mins[i] <- ladlasso.nu.cv[[i]]$metrics_and_info$which.BIC.min
  }
  #store BEST ladlasso result plus all seeds
  ###below is used to check that seeds are regenerated properly and not uniform
  return(list(BICs = ladlasso.BIC.mins , 
              which.BICs = ladlasso.which.BIC.mins ,
              mpes = ladlasso.nu.cv.mpe , 
              #seeds.ridge = ladlasso.seeds.ridge , 
              #seeds.prenu = ladlasso.seeds.prenu ,
              #seeds.nu = ladlasso.seeds.nu ,  
              model = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]] ,
              important = list(diagnostics = data.frame(cbind(data.seed = tracker[7])) ,
                               #model.seed.ridge = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge)) , 
                               #model.seed.prenu = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                               #model.seed.nu = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$model.seed.nu)) , 
                               coefs = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$coefs , 
                               weights = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$weights ,
                               info = data.frame(cbind(n = tracker[1] , 
                                                       p = tracker[2] , 
                                                       eta.x = tracker[3] , 
                                                       eta.y = tracker[4] , 
                                                       g = tracker[5] , 
                                                       h = tracker[6] , 
                                                       data.seed = tracker[7] ,
                                                       #model.seed.ridge = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$model.seed.ridge , 
                                                       #model.seed.prenu = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$model.seed.prenu , 
                                                       #model.seed.nu = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$model.seed.nu) ,
                                                       lambda = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$lambda ,
                                                       nu = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$nu ,
                                                       mpe = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$mpe , 
                                                       mpe.sd = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$mpe.sd , 
                                                       fpr = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$fpr , 
                                                       fnr = ladlasso.nu.cv[[which.min(ladlasso.nu.cv.mpe)]]$metrics_and_info$fnr
                               )
                               )
              )
  )
  )
}

#run across full dataset
ladlasso.HD.ex1ey2 <- HD.ex1ey2 %>%   
  map(safely(ladlasso.sim.fnct))

saveRDS(ladlasso.HD.ex1ey2 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/ladlasso_HD_ex1ey2_500.RData")


