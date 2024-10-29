aleCI_get_mean_and_variance <- function(alist) 
    {
    ab <- as.data.frame(alist[[1]])
    aa_mean <- vector()
    aa_sd <- vector()
    idx <- 1
    for (i in aa[[1]]$x.values) {
        b <- sapply(alist, function(x) x$f.values[x$x.values == i])
        aa_mean[idx] <- mean(b)
        aa_sd[idx] <- sd(b)
        idx <- 1 + idx
        }
    ab$mean <- aa_mean
    ab$sd <- aa_sd
    return(ab)
    }
