HD.data <- readRDS("/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/HDSparsedata_112320.RData")

HD.n200p190 <- HD.data[1:500]
HD.n200p200 <- HD.data[501:1000]
HD.n200p210 <- HD.data[1001:1500]
HD.n200p500 <- HD.data[1501:2000]
HD.n200p1000 <- HD.data[2001:2500]
HD.ex1ey0 <- HD.data[2501:3000]
HD.ex2ey0 <- HD.data[3001:3500]
HD.ex0ey1 <- HD.data[3501:4000]
HD.ex1ey1 <- HD.data[4001:4500]
HD.ex2ey1 <- HD.data[4501:5000]
HD.ex0ey2 <- HD.data[5001:5500]
HD.ex1ey2 <- HD.data[5501:6000]
HD.ex2ey2 <- HD.data[6001:6500]
HD.g2h0 <- HD.data[6501:7000]
HD.g0h2 <- HD.data[7001:7500]
HD.g2h2 <- HD.data[7501:8000]

saveRDS(HD.n200p190 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_n200p190.RData")
saveRDS(HD.n200p200 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_n200p200.RData")
saveRDS(HD.n200p210 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_n200p210.RData")
saveRDS(HD.n200p500 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_n200p500.RData")
saveRDS(HD.n200p1000 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_n2001000.RData")
saveRDS(HD.ex1ey0 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_ex1ey0.RData")
saveRDS(HD.ex2ey0 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_ex2ey0.RData")
saveRDS(HD.ex0ey1 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_ex0ey1.RData")
saveRDS(HD.ex1ey1 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_ex1ey1.RData")
saveRDS(HD.ex2ey1 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_ex2ey1.RData")
saveRDS(HD.ex0ey2 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_ex0ey2.RData")
saveRDS(HD.ex1ey2 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_ex1ey2.RData")
saveRDS(HD.ex2ey2 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_ex2ey2.RData")
saveRDS(HD.g2h0 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_g2h0.RData")
saveRDS(HD.g0h2 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_g0h2.RData")
saveRDS(HD.g2h2 , "/Users/Matt Multach/Dropbox/USC_Grad2/Courses/Dissertation/Dissertation_Git/Data_Storage/Split_by_n/HD_g2h2.RData")

