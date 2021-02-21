#libraries
library(mvtnorm)
library(magrittr)
library(purrr)

#load applied data
#Efron diabetes data (): 
# #442 obs, 10 covs
efron.data <- read.delim("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/diab_Efron_dat" , 
                         header = T , sep = "")
#GDP growth data (koenker & machado, 1999):
# #161 obs, 13 covs
library(quantreg)
data(barro)
#riboflavin data
riboflavin.data <- read.csv("/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/riboflavin_1001.csv")

#generate seeds
set.seed(501)
appliedcv.seeds <- sample(x = c(1:1000000) , size = 100 , replace = FALSE)

#k-fold subsetting function
kfold_subsetter <- function(data , k , seed = 7 , list = FALSE) {
        
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
                newdata <- cbind(data , subset = sample(subset.indicator))
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

#initiate list of datasets
efron.split <- list()
gdp.split <- list()
ribo.split <- list()
#populate list with 3-fold-subset data based on seeds
for(i in 1:length(appliedcv.seeds)) {
        cat("i = " , i , "\n")
        #initialize dataframes in each 
        efron.split[i]$track <- numeric()
        efron.split[i]$train <- matrix()
        efron.split[i]$test <- matrix()
        gdp.split[i]$track <- numeric()
        gdp.split[i]$train <- matrix()
        gdp.split[i]$test <- matrix()
        ribo.split[i]$track <- numeric()
        ribo.split[i]$train <- matrix()
        ribo.split[i]$test <- matrix()
        #create k-fold-indexed data for each dataset
        efron.kfold <- kfold_subsetter(efron.data , k = 3 , 
                                       seed = appliedcv.seeds[i])
        gdp.kfold <- kfold_subsetter(barro , k = 3 ,
                                       seed = appliedcv.seeds[i])
        ribo.kfold <- kfold_subsetter(riboflavin.data , k = 3 ,
                                       seed = appliedcv.seeds[i])
        cat("i = " , i , "\n")
        efron.split[[i]]$track <- i
        efron.split[[i]]$train <- efron.kfold[(efron.kfold$subset != 3) , ]
        efron.split[[i]]$test <- efron.kfold[(efron.kfold$subset == 3) , ]
        gdp.split[[i]]$track <- i
        gdp.split[[i]]$train <- gdp.kfold[(gdp.kfold$subset != 3) , ]
        gdp.split[[i]]$test <- gdp.kfold[(gdp.kfold$subset == 3) , ]
        ribo.split[[i]]$track <- i
        ribo.split[[i]]$train <- ribo.kfold[(ribo.kfold$subset != 3) , ]
        ribo.split[[i]]$train <- ribo.split[[i]]$train[ , -c(1,1003)]
        ribo.split[[i]]$test <- ribo.kfold[(ribo.kfold$subset == 3) , ]
        ribo.split[[i]]$test <- ribo.split[[i]]$test[ , -c(1,1003)]
        
}


#save data
saveRDS(appliedcv.seeds , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/appliedsplit_seeds.RData")
saveRDS(efron.split , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/efron_split.RData")
saveRDS(gdp.split , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/gdp_split.RData")
saveRDS(ribo.split , "/Users/Matt/Dropbox/USC_Grad2/Courses/Dissertation/Applied_Datasets/ribo_split.RData")











