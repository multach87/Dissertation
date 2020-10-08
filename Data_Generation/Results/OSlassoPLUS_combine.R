#load full/half simulated data - for filling in data condition information if there was an error
##since only the error info will be present in that part of the simulated model object
#debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
half.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/500_data_10052020.RData")
#load simulation data
OS.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/OS_resultmain_500.RData")

#initialize dataframe
OS.results <- data.frame(matrix(ncol = ncol(OS.final[[1]]$info)))
colnames(OS.results) <- colnames(OS.final[[1]]$info)

#fill results
##ERRORS AT: c(189 , 1610 , 1671 , 2881 , 3892 , 4036 , 
### 4504 , 4530 , 4531 , 4561 , 4574 , 4587 , 4608 , 4640 , 4664 , 
### 4716 , 4760 , 4788 , 4799 , 4864 , 4892 , 4967 , 4990 , 4997 , 
### 5033 , 5103 , 5119 , 5141 , 5143 , 5226 , 5425 , 5427 , 5485 , 
### 5525 , 5641 , 5652 , 5735 , 5742 , 5861 , 5871 , 6083 , 6206 , 
### 6216 , 6217 , 6217 , 6240 , 6318 , 6571 , 6616 , 6692 , 6823 , 
### 6971 , 7026 , 7028 , 7076 , 7170 , 7284 , 7340 , 7440 , 7445 , 
### 7502 , 7573 , 7638 , 7668 , 7733 , 7818 , 7849 , 8053 , 8057 , 
### 8175 , 8327 , 8547 , 8666 , 8689 , 8701 , 8848 , 8912 , 36460 , 
### 36604 , 36707 , 36947 , 37063 , 37330 , 37558 , 37584 , 37620 , 
### 37645 , 37712 , 37737 , 37751 , 37846 , 37883 , 37919 , 37922 , 
### 37968 , 37982 . 38006 , 38086 , 38097 , 38159 , 38170 , 38213 , 
### 38262 , 38263 , 38296 , 38318 , 38474 , 38511 , 38513 , 38554 , 
### 38560 , 38695 , 38796 , 38808 , 38967 , 38971 , 38984)
for(i in 1:length(OS.final)) {
  if(is.null(OS.final[[i]]$error)) {
    OS.results[i , ] <- OS.final[[i]]$info
  } else {
    cat("error at i = " , i , "\n")
    OS.results[i , 1:7] <- half.data[[i]]$conditions
  }
}

mean(OS.results[ , "fpr"] , na.rm = T)
mean(OS.results[ , "fnr"] , na.rm = T)
mean(OS.results[!is.infinite(OS.results[ , "mpe"]) , "mpe"] , na.rm = T)


#check on error conditions
View(OS.results[c(189 , 1610 , 1671 , 2881 , 3892 , 4036 , 
                  4504 , 4530 , 4531 , 4561 , 4574 , 4587 , 4608 , 4640 , 4664 , 
                  4716 , 4760 , 4788 , 4799 , 4864 , 4892 , 4967 , 4990 , 4997 , 
                  5033 , 5103 , 5119 , 5141 , 5143 , 5226 , 5425 , 5427 , 5485 , 
                  5525 , 5641 , 5652 , 5735 , 5742 , 5861 , 5871 , 6083 , 6206 , 
                  6216 , 6217 , 6240 , 6318 , 6571 , 6616 , 6692 , 6823 , 
                  6971 , 7026 , 7028 , 7076 , 7170 , 7284 , 7340 , 7440 , 7445 , 
                  7502 , 7573 , 7638 , 7668 , 7733 , 7818 , 7849 , 8053 , 8057 , 
                  8175 , 8327 , 8547 , 8666 , 8689 , 8701 , 8848 , 8912 , 36460 , 
                  36604 , 36707 , 36947 , 37063 , 37330 , 37558 , 37584 , 37620 , 
                  37645 , 37712 , 37737 , 37751 , 37846 , 37883 , 37919 , 37922 , 
                  37968 , 37982 , 38006 , 38086 , 38097 , 38159 , 38170 , 38213 , 
                  38262 , 38263 , 38296 , 38318 , 38474 , 38511 , 38513 , 38554 , 
                  38560 , 38695 , 38796 , 38808 , 38967 , 38971 , 38984) , ])

saveRDS(OS.results , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/OSLassoPLUS_resultDF_500.RData")

