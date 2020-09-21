debug.data <- readRDS("/Users/Matt Multach/Desktop/Dissertation/Dissertation_Git/Data_Generation/Data_Storage/debug_data_091720.RData")
ladlasso.debug <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Error_Storage/ladlasso_debug.RData")
ladlasso.final <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/MainResults_Storage/ladlasso_resultmain_DEBUG.RData")

#LAD Lasso: errors at c(102 , 111 , 112 , 132 , 136 , 139 , 168 , 172 , 228 , 293 , 325 , 762)
ladlasso.debug[[102]]
#p > n
debug.data[[102]]$conditions
debug.data[[111]]$conditions
debug.data[[112]]$conditions
debug.data[[132]]$conditions
debug.data[[136]]$conditions
debug.data[[139]]$conditions
debug.data[[168]]$conditions
debug.data[[172]]$conditions
debug.data[[762]]$conditions
#???
debug.data[[228]]$conditions
debug.data[[293]]$conditions
debug.data[[325]]$conditions

