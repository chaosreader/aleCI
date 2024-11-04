aleCI_get_mean_and_variance <- function(alist, mid.ci.fun, low.ci.fun, high.ci.fun) 
    {
    ab <- as.data.frame(alist[[1]])
    aa_mean <- vector()
    aa_low <- vector()
    aa_high <- vector()
    idx <- 1
    for (i in alist[[1]]$x.values) {
        b <- sapply(alist, function(x) x$f.values[x$x.values == i])
        print(b)
        aa_mean[idx] <- mid.ci.fun(b)
        aa_low[idx] <- low.ci.fun(b)
        aa_high[idx] <- high.ci.fun(b)
        idx <- 1 + idx
        }
    ab$mean <- aa_mean
    ab$low <- aa_low
    ab$high <- aa_high
    return(ab)
    }
