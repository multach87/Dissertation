HD.full <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HDSparsedata_112320.RData")

set.seed(501)
debug.data.indices <- c(sample(x = c(1:500) , size = 10 , replace = FALSE) , 
                       sample(x = c(501:1000) , size = 10 , replace = FALSE) , 
                       sample(x = c(1001:1500) , size = 10 , replace = FALSE) , 
                       sample(x = c(1501:2000) , size = 10 , replace = FALSE) , 
                       sample(x = c(2001:2500) , size = 10 , replace = FALSE) , 
                       sample(x = c(2501:3000) , size = 10 , replace = FALSE) , 
                       sample(x = c(3001:3500) , size = 10 , replace = FALSE) , 
                       sample(x = c(3501:4000) , size = 10 , replace = FALSE) , 
                       sample(x = c(4001:4500) , size = 10 , replace = FALSE) , 
                       sample(x = c(4501:5000) , size = 10 , replace = FALSE) , 
                       sample(x = c(5001:5500) , size = 10 , replace = FALSE) , 
                       sample(x = c(5501:6000) , size = 10 , replace = FALSE) , 
                       sample(x = c(6001:6500) , size = 10 , replace = FALSE) , 
                       sample(x = c(6501:7000) , size = 10 , replace = FALSE) , 
                       sample(x = c(7001:7500) , size = 10 , replace = FALSE) , 
                       sample(x = c(7501:8000) , size = 10 , replace = FALSE) )


HD.debug.data <- HD.full[debug.data.indices]
saveRDS(HD.debug.data , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HD_debug_data_11302020.RData")

