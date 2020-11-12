half.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/500_data_10052020.RData")

outlier25.data <- half.data[1:9000]
saveRDS(outlier25.data , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/outlier25.RData")

outlier50.data <- half.data[9001:18000]
saveRDS(outlier50.data , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/outlier50.RData")

outlier100.data <- half.data[18001:27000]
saveRDS(outlier100.data , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/outlier100.RData")

outlier200.data <- half.data[27001:36000]
saveRDS(outlier200.data , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/outlier200.RData")

distr25.data <- half.data[36001:39000]
saveRDS(distr25.data , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/distr25.RData")

distr50.data <- half.data[39001:42000]
saveRDS(distr50.data , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/distr50.RData")

distr100.data <- half.data[42001:45000]
saveRDS(distr100.data , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/distr100.RData")

distr200.data <- half.data[45001:48000]
saveRDS(distr200.data , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/distr200.RData")
