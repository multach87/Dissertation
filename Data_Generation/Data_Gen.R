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
sim.structure1 <- as.data.frame(matrix(ncol = 6 , nrow = 96))
{
       colnames(sim.structure1) <- c("n" , "p" , "eta.x" , "eta.y" , "g" , "h")
       sim.structure1[ , "eta.x"] <- c(rep(c(0.0 , 0.1 , 0.2) , 24) , 
                                       rep(0 , 24))
       sim.structure1[ , "eta.y"] <- c(rep(c(rep(0.0 , 3) , rep(0.1 , 3) , rep(0.2 , 3)) , 8) , 
                                       rep(0 , 24))
       sim.structure1[ , "p"] <- c(rep(c(rep(8 , 9) , rep(30 , 9)) , 4) , 
                                   rep(c(rep(8 , 3) , rep(30 , 3)) , 4))
       sim.structure1[ , "n"] <- c(rep(25 , 18) , rep(50 , 18) , rep(100 , 18) , rep(200 , 18) , 
                                   rep(25 , 6) , rep(50 , 6) , rep(100 , 6) , rep(200 , 6))
       sim.structure1[ , "g"] <- c(rep(0 , 72) , rep(c(0.2 , 0.0 , 0.2) , 8))
       sim.structure1[ , "h"] <- c(rep(0 , 72) , rep(c(0.0 , 0.2 , 0.2) , 8))
}
View(sim.structure1)
#generate repped conditions dataframe
sim.structure.repped <- as.data.frame(matrix(ncol = 7 , nrow = (96*1000)))
colnames(sim.structure.repped) <- c("n" , "p" , "eta.x" , "eta.y" , "g" , "h" , "seed")
for(i in 1:nrow(sim.structure1)) {
       sim.structure.repped[ ((1000*(i - 1)) + 1): (1000*i), (1:6)] <- 
              purrr::map_dfr(seq_len(1000) , ~sim.structure1[i , ])
}
#create checks indices just in case
checks.index <- numeric(192)
for(i in 1:96) {
       checks.index[((2*i) - 1)] <- (((i-1)*1000) + 1)
       checks.index[(2*i)] <- (i*1000)
}
checks.index
#get seed from sim.structure rather than internally-generating
sim.structure.repped[ , "seed"] <- rnorm((96*1000))
head(sim.structure.repped)
#data-generating function
data.gen <- function(n , p , eta.x , eta.y , g , h , seed) {      
       conditions <- data.frame(n = n , p = p , eta.x = eta.x , eta.y = eta.y , 
                                g = g , h = h)
       betas <- matrix(0 , nrow = p , ncol = 1)
       betas[1,1] <- 0.5
       betas[2,1] <- 1.0
       betas[3,1] <- 1.5
       betas[4,1] <- 2.0
       seed <- seed                       #set seed
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
       combine <- list(conditions = conditions , Y = Y , X = X , err = err)        #create combined list of all values
       return(combine)                       #save combined list of all values
}
#map data.gen over all iterations of all data conditions
data.full <- sim.structure.repped %>%   
       pmap(data.gen)

#save data to computer
#saveRDS(data.full , "/Users/Matt Multach/Desktop/Quals_Data/Data/sim_data_110119.RData")










