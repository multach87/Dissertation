elnet.seeds <- data.frame(matrix(ncol = 102 * 6 * 3 , nrow = 96000))

seeds <- rnorm(nrow(elnet.seeds) * ncol(elnet.seeds))

elnet.seeds <- matrix(seeds , ncol = (102 * 6 * 3))

seeds2 <- sample(x = c(0:(2 * nrow(elnet.seeds) * ncol(elnet.seeds))) , 
                size = (2 * nrow(elnet.seeds) * ncol(elnet.seeds)) , replace = FALSE)


