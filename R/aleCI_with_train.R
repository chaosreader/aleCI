aleCI_with_train <- function(X, X.models, pred.fun, mid.ci.fun, low.ci.fun, high.ci.fun, J, K, NA.plot)
    {
    aa <- list()
    for (i in 1:length(X.models)) {
        aa[[i]] <- aleCI_origional(X[[length(X)]], X.models[[i]], yhat, J, K, NA.plot)
        }
    ab <- aleCI_get_mean_and_variance(aa, mid.ci.fun, low.ci.fun, high.ci.fun)
    ac <- aleCI_origional(X[[1]], X.models[[1]], yhat, J, K, NA.plot)
    ad <- list()
    ad[[1]] <- as.data.frame(ac)
    ad[[2]] <- ab
    return(ad)
    }
