aleCI_plot <- function(X, X.models, pred.fun, mid.ci.fun, low.ci.fun, high.ci.fun, J, K=40, NA.plot = TRUE)
    {
    ab <- aleCI(X, X.models, pred.fun, mid.ci.fun, low.ci.fun, high.ci.fun, J, K, NA.plot)
    aleCI_make_plot(ab, X, J)
    }
