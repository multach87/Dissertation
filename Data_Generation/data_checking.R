#check seeds for matches
##extract "conditions"
library(data.table)
conds <- rbindlist(lapply(data.full , "[[" , "conditions"))
###using for loop
{
       match.binary <- matrix(nrow = 96000 , ncol = 3) #create matrix of seeds and 0/1 for any match
       match.binary[ , 1] <- unlist(conds[ , "seed"])
       for(i in 1:nrow(match.binary)) {                #quick version, only checking each i against -i > i
              if(match.binary[i , 1] %in% match.binary[(-i > i) , 1]) {
                     match.binary[i , 2] <- 1
              }  else {
                     match.binary[i , 2] <- 0
              }
              cat("i = " , i , "\n")
       }
       1 %in% match.binary[ , 2]
       for(i in 1:nrow(match.binary)) {                #slow version, checking each i against all -i
              if(match.binary[i , 1] %in% match.binary[-i , 1]) {
                     match.binary[i , 3] <- 1
              }  else {
                     match.binary[i , 3] <- 0
              }
              cat("i = " , i , "\n")
       }
       1 %in% match.binary[ , 3]
}
###not using loops

#check n and p for each dataset

