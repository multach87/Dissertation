#libraries
library(mvtnorm)
library(magrittr)
library(purrr)

#g-and-h distribution
ghdist<-function(n,g=0,h=0){
       #
       # generate n observations from a g-and-h dist.
       #
       x<-rnorm(n)
       if (g>0){
              ghdist<-(exp(g*x)-1)*exp(h*x^2/2)/g
       }
       if(g==0)ghdist<-x*exp(h*x^2/2)
       ghdist
}

#generate initial 108 data conditions
sim.structure1 <- as.data.frame(matrix(ncol = 6 , nrow = 16))
{
       colnames(sim.structure1) <- c("n" , "p" , "eta.x" , "eta.y" , "g" , "h")
        sim.structure1[ , "n"] <- c(rep(200 , 16)) 
        sim.structure1[ , "p"] <- c(190 , 200 , 210 , 500 , rep(1000 , 12))
        sim.structure1[ , "eta.x"] <- c(rep(0 , 4) , rep(c(0.0 , 0.1 , 0.2) , 3) , 
                                        rep(0 , 3))
        sim.structure1[ , "eta.y"] <- c(rep(0 , 4) , rep(0 , 3) , rep(0.1 , 3) , 
                                        rep(0.2 , 3) , rep(0 , 3))
        sim.structure1[ , "g"] <- c(rep(0 , 13) , 0.2 , 0.0 , 0.2)
        sim.structure1[ , "h"] <- c(rep(0 , 13) , 0.0 , 0.2 , 0.2)
}
View(sim.structure1)
#generate repped conditions dataframe
sim.structure.repped <- as.data.frame(matrix(ncol = 7 , nrow = (16*500)))
colnames(sim.structure.repped) <- c("n" , "p" , "eta.x" , "eta.y" , "g" , "h" , "seed")
for(i in 1:nrow(sim.structure1)) {
       sim.structure.repped[ ((500*(i - 1)) + 1): (500*i), (1:6)] <- 
              purrr::map_dfr(seq_len(500) , ~sim.structure1[i , ])
}
#create checks indices just in case
checks.index <- numeric(32)
for(i in 1:16) {
       checks.index[((2*i) - 1)] <- (((i-1)*500) + 1)
       checks.index[(2*i)] <- (i*500)
}
checks.index
#get seed from sim.structure rather than internally-generating
seeds <- rnorm(16*500)

sim.structure.repped[ , 7] <- seeds

head(sim.structure.repped)
#data-generating function
data.gen.HD <- function(n , p , eta.x , eta.y , g , h , seed.1) {      
       conditions <- data.frame(n = n , p = p , eta.x = eta.x , eta.y = eta.y , 
                                g = g , h = h , seed = seed.1)
       #print tracker of status
       cat("n = " , conditions[1 , 1] , " , p = " , conditions[1 , 2] ,
           " , eta.x = " , conditions[1 , 3] , " , eta.y = " , conditions[1 , 4] ,
           " , g = " , conditions[1 , 5] , " , h = " , conditions[1 , 6] , 
           " , seed = " , conditions[1 , 7] , ";\n")
       betas <- matrix(0 , nrow = p , ncol = 1)
       betas[1,1] <- 0.5
       betas[2,1] <- 1.0
       betas[3,1] <- 1.5
       betas[4,1] <- 2.0
       seed <- seed.1                       #set seed
       covar.X <- matrix(rep(0 , p^2) , ncol = p)  #generate covariance matrix
       diag(covar.X) <- 1                          #1's along cavariance diagonal
       X.UC <- rmvnorm(floor((1 - eta.x)*n) , mean = rep(0 , p) , sigma = covar.X)
       #generate uncontam. X values
       if(((g == 0) & (h == 0))){
              if(eta.x > 0) {                             #generate contam. X values
                     X.C <- rmvnorm(ceiling(eta.x*n) , mean <- rep(10 , p) , sigma = covar.X)
                     X <- rbind(X.UC , X.C)
              } else {
                     X.C <- 0
                     X <- X.UC
              }
              err.UC <- rnorm(floor((1-eta.y)*n) , mean = 0 , sd = 1)   #generate uncontom. residuals
              if(eta.y > 0) {                                           #generate contam. residuals
                     err.C <- rnorm(ceiling(eta.y*n) , mean = 2 , sd = 5)
                     err <- c(err.UC , err.C)
              } else {
                     err.c <- 0
                     err <- err.UC
              }
       } else if(((g != 0) | (h != 0))) {
              X <- X.UC
              err <- ghdist(n = n , g = g , h = h)
       }
       Y <- X %*% betas[ , 1] + err                                    #generate Y values
       combine <- list(conditions = conditions ,
                       betas = betas , 
                       Y = Y , 
                       X = X , 
                       err = err)        #create combined list of all values
       return(combine)                       #save combined list of all values
}

#map data.gen.HD over all iterations of all data conditions
data.HD <- sim.structure.repped %>%   
        pmap(data.gen.HD)


#save data to computer
saveRDS(data.HD , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HDSparsedata_112320.RData")










