aleCI_class_numeric <- function(X, X.model, pred.fun, J, K) {
    arr <- aleCI_make_buckets(X, J, K, d, N)
    z <- arr$z
    K <- arr$K
    fJ <- arr$fJ
    a1 <- arr$a1
    X1 = X
    X2 = X
    X1[,J] = z[a1]
    X2[,J] = z[a1+1]
    y.hat1 = pred.fun(X.model=X.model, newdata = X1)
    y.hat2 = pred.fun(X.model=X.model, newdata = X2)
    Delta=y.hat2-y.hat1  #N-length vector of individual local effect values
    Delta = as.numeric(tapply(Delta, a1, mean)) #K-length vector of averaged local effect values
    fJ = c(0, cumsum(Delta)) #K+1 length vector
    #now vertically translate fJ, by subtracting its average (averaged across X[,J])
    b1 <- as.numeric(table(a1)) #frequency count of X[,J] values falling into z intervals
    fJ = fJ - sum((fJ[1:K]+fJ[2:(K+1)])/2*b1)/sum(b1)
    x <- z
#    plot(x, fJ, type="l", xlab=paste("x_",J, " (", names(X)[J], ")", sep=""), ylab= paste("f_",J,"(x_",J,")", sep=""))
    return(list(x = x, fJ = fJ))
}
