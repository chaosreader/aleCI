#' @export
aleCI_origional  <- function(X, X.model, pred.fun, J, K = 40, NA.plot = TRUE) {

N = dim(X)[1]  #sample size
d = dim(X)[2]  #number of predictor variables

if (length(J) == 1) { #calculate main effects ALE plot
  
  if (class(X[,J]) == "factor") {
      
      arr <- aleCI_class_factor(X, X.model, pred.fun, J, K, d, N)
      x <- arr$x
      fJ <- arr$fJ
  } #end of if (class(X[,J]) == "factor") statement

  else
      
      arr <- aleCI_class_numeric(X, X.model, pred.fun, J, K)
      x <- arr$x
      fJ <- arr$fJ
  }  #end of else if (class(X[,J]) == "numeric" | class(X[,J]) == "integer") statement


else print("error:  J must be a vector of length")

list(K=K, x.values=x, f.values = fJ)
}

