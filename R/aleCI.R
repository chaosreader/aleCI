aleCI <- function(X, X.models, pred.fun, mid.ci.fun, low.ci.fun, high.ci.fun, J, K=40, NA.plot = TRUE) {
    valid_list_length_2 <- FALSE
    valid_list_length_1 <- FALSE
    
    if ((class(X) == "list") & (class(X[[1]]) == "data.frame") & (class(X[[2]]) == "data.frame"))
    {
        valid_list_length_2 <- TRUE
    }
    else if ((class(X) == "list") & (class(X[[1]]) == "data.frame"))
    {
        valid_list_length_1 <- TRUE
    }
    else if (class(X) == "data.frame")
    {
        XX <- list()
        XX[[1]] <- X
        X <- XX
        valid_list_length_1 <- TRUE
    }
    else 
    {
        print("data must be either dataframe or list of dataframes")
    }
    
    if (class(X.models) != "list") 
    {
        XX.models <- list()
        XX.models[[1]] <- X.models
        X.models <- X.models
    }
        
    if (valid_list_length_2)
    {
        ab <- aleCI_with_train(X, X.models, pred.fun, mid.ci.fun, low.ci.fun, high.ci.fun, J, K, NA.plot)
    }
    else
    {
        ab <- aleCI_with_train(X, X.models, pred.fun, mid.ci.fun, low.ci.fun, high.ci.fun, J, K, NA.plot)
    }
    return(ab)
    }
