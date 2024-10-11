aleCI_class_factor <- function(X, X.model, pred.fun, J, K, d, N) {
#for categorical X[,J], calculate the ALE plot
  #Get rid of any empty levels of x and tabulate level counts and probabilities
    arr <- aleCI_make_buckets(X, J, K, d, N)
    X <- arr$X
    x.prob <- arr$x.prob
    x.count <- arr$x.count
    K <- arr$K
    
    D.cum <- aleCI_distance_matrix(X, J, K, d, x.count)
    
    #calculate the 1-D MDS representation of D and the ordered levels of X[,J]
    D1D <- cmdscale(D.cum, k = 1) #1-dimensional MDS representation of the distance matrix
    ind.ord <- sort(D1D, index.return = T)$ix    #K-length index vector. The i-th element is the original level index of the i-th lowest ordered level of X[,J].
    ord.ind <- sort(ind.ord, index.return = T)$ix    #Another K-length index vector. The i-th element is the order of the i-th original level of X[,J].
    levs.orig <- levels(X[,J])  #as.character levels of X[,J] in original order
    levs.ord <- levs.orig[ind.ord]  #as.character levels of X[,J] after ordering
    x.ord <- ord.ind[as.numeric(X[,J])]  #N-length vector of numerical version of X[,J] with numbers corresponding to the indices of the ordered levels

    #Calculate the model predictions with the levels of X[,J] increased and decreased by one
    row.ind.plus <- (1:N)[x.ord < K]  #indices of rows for which X[,J] was not the highest level
    row.ind.neg <- (1:N)[x.ord > 1]  #indices of rows for which X[,J] was not the lowest level
    X.plus <- X
    X.neg <- X
    X.plus[row.ind.plus,J] <- levs.ord[x.ord[row.ind.plus]+1]  #Note that this leaves the J-th column as a factor with the same levels as X[,J], whereas X.plus[,J] <- . . . would convert it to a character vector
    X.neg[row.ind.neg,J] <- levs.ord[x.ord[row.ind.neg]-1]
    y.hat <- pred.fun(X.model=X.model, newdata = X)
    y.hat.plus <- pred.fun(X.model=X.model, newdata = X.plus[row.ind.plus,])
    y.hat.neg <- pred.fun(X.model=X.model, newdata = X.neg[row.ind.neg,])

    #Take the appropriate differencing and averaging for the ALE plot
    Delta.plus <- y.hat.plus-y.hat[row.ind.plus]  #N.plus-length vector of individual local effect values. They are the differences between the predictions with the level of X[,J] increased by one level (in ordered levels) and the predictions with the actual level of X[,J].
    Delta.neg <- y.hat[row.ind.neg]-y.hat.neg  #N.neg-length vector of individual local effect values. They are the differences between the predictions with the actual level of X[,J] and the predictions with the level of X[,J] decreased (in ordered levels) by one level. 
    Delta <- as.numeric(tapply(c(Delta.plus, Delta.neg), c(x.ord[row.ind.plus], x.ord[row.ind.neg]-1), mean)) #(K-1)-length vector of averaged local effect values corresponding to the first K-1 ordered levels of X[,J]. 
    fJ <- c(0, cumsum(Delta)) #K length vector of accumulated averaged local effects
    #now vertically translate fJ, by subtracting its average (averaged across X[,J])
    fJ = fJ - sum(fJ*x.prob[ind.ord])
    x <- levs.ord
#    barplot(fJ, names=x, xlab=paste("x_", J, " (", names(X)[J], ")", sep=""), ylab= paste("f_",J,"(x_",J,")", sep=""), las =3)
    return(list(x = x, fJ = fJ))
}
