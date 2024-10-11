aleCI_make_buckets <- function(X, J, K, d, N) {
    if  (class(X[,J]) == "factor") {
        X[,J] <- droplevels(X[,J])
        x.count <- as.numeric(table(X[,J])) #frequency count vector for levels of X[,J]
        x.prob <- x.count/sum(x.count) #probability vector for levels of X[,J]
        K <- nlevels(X[,J])  #reset K to the number of levels of X[,J]
        return(list(X = X, x.prob = x.prob, x.count = x.count, K = K))
        }
    else {
        #find the vector of z values corresponding to the quantiles of X[,J]
        z= c(min(X[,J]), as.numeric(quantile(X[,J],seq(1/K,1,length.out=K), type=1)))  #vector of K+1 z values
        z = unique(z)  #necessary if X[,J] is discrete, in which case z could have repeated values 
        K = length(z)-1 #reset K to the number of unique quantile points
        fJ = numeric(K)
        #group training rows into bins based on z
        a1=as.numeric(cut(X[,J], breaks=z, include.lowest=TRUE)) #N-length index vector indicating into which z-bin the training rows fall
        return(list(z = z, K = K, fJ = fJ, a1 = a1))
        }
    }
