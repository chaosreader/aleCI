aleCI_distance_matrix <- function(X, J, K, d, x.count) {
    
    D.cum <- matrix(0, K, K)  #will be the distance matrix between pairs of levels of X[,J]
    D <- matrix(0, K, K)  #initialize matrix

    #For loop for calculating distance matrix D for each of the other predictors
    for (j in setdiff(1:d, J)) { 
      if (class(X[,j]) == "factor") {#Calculate the distance matrix for each categorical predictor
        A=table(X[,J],X[,j])  #frequency table, rows of which will be compared
        A=A/x.count
        for (i in 1:(K-1)) {
          for (k in (i+1):K) {
            D[i,k] = sum(abs(A[i,]-A[k,]))/2  #This dissimilarity measure is always within [0,1]
            D[k,i] = D[i,k]
          }
        }
        D.cum <- D.cum + D
      }  #End of if (class(X[,j] == "factor") statement
      else  { #calculate the distance matrix for each numerical predictor
        q.x.all <- quantile(X[,j], probs = seq(0, 1, length.out = 100), na.rm = TRUE, names = FALSE)  #quantiles of X[,j] for all levels of X[,J] combined
        x.ecdf=tapply(X[,j], X[,J], ecdf) #list of ecdf's for X[,j] by levels of X[,J]
        for (i in 1:(K-1)) {
          for (k in (i+1):K) {
            D[i,k] = max(abs(x.ecdf[[i]](q.x.all)-x.ecdf[[k]](q.x.all)))  #This dissimilarity measure is the Kolmogorov-Smirnov distance between X[,j] for levels i and k of X[,J]. It is always within [0,1]
            D[k,i] = D[i,k]
          }
        }
        D.cum <- D.cum + D
      }  #End of else statement that goes with if (class(X[,j] == "factor") statement
    } #end of for (j in setdiff(1:d, J) loop

    return(D.cum)
}

