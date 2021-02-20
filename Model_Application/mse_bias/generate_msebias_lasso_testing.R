#libraries
library(mvtnorm)
library(magrittr)
library(purrr)

#load test set
test500.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/testset_500_021721.RData")

#load model
lasso.full <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/lasso_full.RData")
#take out half dataset
set.seed(501)
half_data.indices <- c(sample(x = c(1:1000) , size = 500 , replace = FALSE) , 
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
lasso500.data <- lasso.full[half_data.indices]
##save for later results use and remove full data
saveRDS(lasso500.data , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Full_results/lasso_500.RData")
rm(list = c("lasso.full"))

combined.data <- list()
#combine data
for(i in 1:2) {
  combined.data[[i]] <- c(lasso500.data[[i]] , test500.data[[i]])
}

#clear separate data so memory isn't taken up
rm(list = c("lasso500.data" , "test500.data"))

msebias <- function(data) {
  conditions <- as.vector(unlist(data$conditions))
  cat("n = " , conditions[1] , " , p = " , conditions[2] ,
      " , eta.x = " , conditions[3] , " , eta.y = " , conditions[4] ,
      " , g = " , conditions[5] , " , h = " , conditions[6] ,
      ";\n")
  pred.y <- data$X %*% data$result$model$coefs
  #cat("pred.y = " , pred.y , "\n")
  resid <- data$Y - pred.y
  resid.sq <- resid^2
  sum.resid.sq <- sum(resid.sq)
  mse <- sum.resid.sq / data$result$important$info$n
  
  true.coefs <- c(0.5 , 1.0 , 1.5 , 2.0)
  coefs.dif <- data$result$model$coefs[1:4] - true.coefs
  coefs.dif.sq <- coefs.dif^2
  sum.coefs.dif.sq <- sum(coefs.dif.sq)
  coefs.bias <- sum.coefs.dif.sq / 4
  cat("coefs.bias = " , coefs.bias , "\n")
  
  return(data.frame(cbind(n = conditions[1] ,
                   p = conditions[2] ,
                   eta.x = conditions[3] ,
                   eta.y = conditions[4] ,
                   g = conditions[5] ,
                   h = conditions[6] ,
                   data.seed = conditions[7] ,
                   mse = mse ,
                   coefs.bias = coefs.bias
                   ) 
                   ) 
         )
}

#run across full dataset
lasso.mse.bias.HALF <- combined.data[c(1:2)] %>%   
  map(safely(msebias))

saveRDS(LADlasso.HALF , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/SNCDLADlasso_distr100_500.RData")



#testing
# #MSE
test.model.data <- lasso500.data[[1]]
test.sim.data <- test500.data[[1]]
# # #test combining data
combined.data <- c(test.model.data , test.sim.data)

pred.y <- test.sim.data$X %*% test.model.data$result$model$coefs
resid <- test.sim.data$Y - pred.y
resid.sq <- resid^2
sum.resid.sq <- sum(resid.sq)
mse <- sum.resid.sq / test.model.data$result$important$info$n

# #Coef bias
# # #create coefs vector
true.coefs <- c(0.5 , 1.0 , 1.5 , 2.0)
coefs.dif <- test.model.data$result$model$coefs[1:4] - true.coefs
coefs.dif.sq <- coefs.dif^2
sum.coefs.dif.sq <- sum(coefs.dif.sq)
coefs.bias <- sum.coefs.dif.sq / 4
