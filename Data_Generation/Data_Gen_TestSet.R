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
       sim.structure1[ , "n"] <- c(rep(38 , 18) , rep(75 , 18) , rep(150 , 18) , rep(300 , 18) , 
                                   rep(38 , 6) , rep(75 , 6) , rep(150 , 6) , rep(300 , 6))
       sim.structure1[ , "g"] <- c(rep(0 , 72) , rep(c(0.2 , 0.0 , 0.2) , 8))
       sim.structure1[ , "h"] <- c(rep(0 , 72) , rep(c(0.0 , 0.2 , 0.2) , 8))
}
View(sim.structure1)
#generate repped conditions dataframe
sim.structure.repped <- as.data.frame(matrix(ncol = 7 , nrow = (96*1000)))
colnames(sim.structure.repped) <- c("n" , "p" , "eta.x" , "eta.y" , "g" , "h" , 
                                    "seed.1")
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
#get seed from original data, then remove full dataset
data.all <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/fulldata_091620.RData")
seeds <- numeric()
for(i in 1:length(data.all)) {
        seeds[i] <- data.all[[i]]$seeds$seed.1
}
rm(list = c("data.all"))

sim.structure.repped[ , 7] <- seeds

head(sim.structure.repped)
#data-generating function
data.gen <- function(n , p , eta.x , eta.y , g , h , seed.1) {      
       conditions <- data.frame(n = n , p = p , eta.x = eta.x , eta.y = eta.y , 
                                g = g , h = h , seed = seed.1)
       #seeds <- data.frame(seed.1 = seed.1)
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
                       seed = seed.1 , 
                       betas = betas , 
                       Y = Y[floor((n * .667) + 1) : n] , 
                       X = X[floor((n * .667) + 1) : n , ] , 
                       err = err[floor((n * .667) + 1) : n])        #create combined list of all values
       return(combine)                       #save combined list of all values
}


#map data.gen over all iterations of all data conditions
full.data <- sim.structure.repped %>%   
       pmap(data.gen)

#save data to computer
saveRDS(full.data , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/testset_021721.RData")

#generate and save half test set data
set.seed(501)
test_data.indices <- c(sample(x = c(1:1000) , size = 500 , replace = FALSE) , 
                        sample(x = c(1001:2000) , size = 500 , replace = FALSE) , 
                        sample(x = c(2001:3000) , size = 500 , replace = FALSE) , 
                        sample(x = c(3001:4000) , size = 500 , replace = FALSE) , 
                        sample(x = c(4001:5000) , size = 500 , replace = FALSE) , 
                        sample(x = c(5001:6000) , size = 500 , replace = FALSE) , 
                        sample(x = c(6001:7000) , size = 500 , replace = FALSE) , 
                        sample(x = c(7001:8000) , size = 500 , replace = FALSE) , 
                        sample(x = c(8001:9000) , size = 500 , replace = FALSE) , 
                        sample(x = c(9001:10000) , size = 500 , replace = FALSE) , 
                        sample(x = c(10001:11000) , size = 500 , replace = FALSE) , 
                        sample(x = c(11001:12000) , size = 500 , replace = FALSE) , 
                        sample(x = c(12001:13000) , size = 500 , replace = FALSE) , 
                        sample(x = c(13001:14000) , size = 500 , replace = FALSE) , 
                        sample(x = c(14001:15000) , size = 500 , replace = FALSE) , 
                        sample(x = c(15001:16000) , size = 500 , replace = FALSE) , 
                        sample(x = c(16001:17000) , size = 500 , replace = FALSE) , 
                        sample(x = c(17001:18000) , size = 500 , replace = FALSE) , 
                        sample(x = c(18001:19000) , size = 500 , replace = FALSE) , 
                        sample(x = c(19001:20000) , size = 500 , replace = FALSE) , 
                        sample(x = c(20001:21000) , size = 500 , replace = FALSE) , 
                        sample(x = c(21001:22000) , size = 500 , replace = FALSE) , 
                        sample(x = c(22001:23000) , size = 500 , replace = FALSE) , 
                        sample(x = c(23001:24000) , size = 500 , replace = FALSE) , 
                        sample(x = c(24001:25000) , size = 500 , replace = FALSE) , 
                        sample(x = c(25001:26000) , size = 500 , replace = FALSE) , 
                        sample(x = c(26001:27000) , size = 500 , replace = FALSE) , 
                        sample(x = c(27001:28000) , size = 500 , replace = FALSE) , 
                        sample(x = c(28001:29000) , size = 500 , replace = FALSE) , 
                        sample(x = c(29001:30000) , size = 500 , replace = FALSE) , 
                        sample(x = c(30001:31000) , size = 500 , replace = FALSE) , 
                        sample(x = c(31001:32000) , size = 500 , replace = FALSE) , 
                        sample(x = c(32001:33000) , size = 500 , replace = FALSE) , 
                        sample(x = c(33001:34000) , size = 500 , replace = FALSE) , 
                        sample(x = c(34001:35000) , size = 500 , replace = FALSE) , 
                        sample(x = c(35001:36000) , size = 500 , replace = FALSE) , 
                        sample(x = c(36001:37000) , size = 500 , replace = FALSE) , 
                        sample(x = c(37001:38000) , size = 500 , replace = FALSE) , 
                        sample(x = c(38001:39000) , size = 500 , replace = FALSE) , 
                        sample(x = c(39001:40000) , size = 500 , replace = FALSE) , 
                        sample(x = c(40001:41000) , size = 500 , replace = FALSE) , 
                        sample(x = c(41001:42000) , size = 500 , replace = FALSE) , 
                        sample(x = c(42001:43000) , size = 500 , replace = FALSE) , 
                        sample(x = c(43001:44000) , size = 500 , replace = FALSE) , 
                        sample(x = c(44001:45000) , size = 500 , replace = FALSE) , 
                        sample(x = c(45001:46000) , size = 500 , replace = FALSE) , 
                        sample(x = c(46001:47000) , size = 500 , replace = FALSE) , 
                        sample(x = c(47001:48000) , size = 500 , replace = FALSE) , 
                        sample(x = c(48001:49000) , size = 500 , replace = FALSE) , 
                        sample(x = c(49001:50000) , size = 500 , replace = FALSE) , 
                        sample(x = c(50001:51000) , size = 500 , replace = FALSE) , 
                        sample(x = c(51001:52000) , size = 500 , replace = FALSE) , 
                        sample(x = c(52001:53000) , size = 500 , replace = FALSE) , 
                        sample(x = c(53001:54000) , size = 500 , replace = FALSE) , 
                        sample(x = c(54001:55000) , size = 500 , replace = FALSE) , 
                        sample(x = c(55001:56000) , size = 500 , replace = FALSE) , 
                        sample(x = c(56001:57000) , size = 500 , replace = FALSE) , 
                        sample(x = c(57001:58000) , size = 500 , replace = FALSE) , 
                        sample(x = c(58001:59000) , size = 500 , replace = FALSE) , 
                        sample(x = c(59001:60000) , size = 500 , replace = FALSE) , 
                        sample(x = c(60001:61000) , size = 500 , replace = FALSE) , 
                        sample(x = c(61001:62000) , size = 500 , replace = FALSE) , 
                        sample(x = c(62001:63000) , size = 500 , replace = FALSE) , 
                        sample(x = c(63001:64000) , size = 500 , replace = FALSE) , 
                        sample(x = c(64001:65000) , size = 500 , replace = FALSE) , 
                        sample(x = c(65001:66000) , size = 500 , replace = FALSE) , 
                        sample(x = c(66001:67000) , size = 500 , replace = FALSE) , 
                        sample(x = c(67001:68000) , size = 500 , replace = FALSE) , 
                        sample(x = c(68001:69000) , size = 500 , replace = FALSE) , 
                        sample(x = c(69001:70000) , size = 500 , replace = FALSE) , 
                        sample(x = c(70001:71000) , size = 500 , replace = FALSE) , 
                        sample(x = c(71001:72000) , size = 500 , replace = FALSE) , 
                        sample(x = c(72001:73000) , size = 500 , replace = FALSE) , 
                        sample(x = c(73001:74000) , size = 500 , replace = FALSE) , 
                        sample(x = c(74001:75000) , size = 500 , replace = FALSE) , 
                        sample(x = c(75001:76000) , size = 500 , replace = FALSE) , 
                        sample(x = c(76001:77000) , size = 500 , replace = FALSE) , 
                        sample(x = c(77001:78000) , size = 500 , replace = FALSE) , 
                        sample(x = c(78001:79000) , size = 500 , replace = FALSE) , 
                        sample(x = c(79001:80000) , size = 500 , replace = FALSE) , 
                        sample(x = c(80001:81000) , size = 500 , replace = FALSE) , 
                        sample(x = c(81001:82000) , size = 500 , replace = FALSE) , 
                        sample(x = c(82001:83000) , size = 500 , replace = FALSE) , 
                        sample(x = c(83001:84000) , size = 500 , replace = FALSE) , 
                        sample(x = c(84001:85000) , size = 500 , replace = FALSE) , 
                        sample(x = c(85001:86000) , size = 500 , replace = FALSE) , 
                        sample(x = c(86001:87000) , size = 500 , replace = FALSE) , 
                        sample(x = c(87001:88000) , size = 500 , replace = FALSE) , 
                        sample(x = c(88001:89000) , size = 500 , replace = FALSE) , 
                        sample(x = c(89001:90000) , size = 500 , replace = FALSE) , 
                        sample(x = c(90001:91000) , size = 500 , replace = FALSE) , 
                        sample(x = c(91001:92000) , size = 500 , replace = FALSE) , 
                        sample(x = c(92001:93000) , size = 500 , replace = FALSE) , 
                        sample(x = c(93001:94000) , size = 500 , replace = FALSE) , 
                        sample(x = c(94001:95000) , size = 500 , replace = FALSE) , 
                        sample(x = c(95001:96000) , size = 500 , replace = FALSE) )

half.data <- full.data[test_data.indices]
saveRDS(half.data , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/testset_500_021721.RData")








